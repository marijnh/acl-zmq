(defpackage :zmq
  (:nicknames :zeromq)
  (:use :cl)
  (:export #:+p2p+ #:+pub+ #:+sub+ #:+req+ #:+rep+ #:+xreq+ #:+xrep+ #:+upstream+ #:+downstream+

           #:+hwm+ #:+swap+ #:+affinity+ #:+identity+ #:+subscribe+ #:+unsubscribe+ #:+rate+
           #:+recovery-ivl+ #:+mcast-loop+ #:+sndbuf+ #:+rcvbuf+ #:+rcvmore+

           #:+noblock+ #:+sndmore+
           #:+pollin+ #:+pollout+ #:+pollerr+

           #:zmq-error #:zmq-error-code #:zmq-again

           #:msg-init #:msg-init-size #:msg-init-string #:msg-init-vector
           #:with-msg #:msg-close #:msg-copy
           #:msg-string #:msg-vector #:msg-size #:msg-data

           #:socket #:socket-close #:with-socket #:bind #:connect
           #:getsocktopt-str #:getsockopt-num #:setsockopt-str #:setsockopt-num
           #:send #:recv #:poll

           #:start-helper #:close-helper #:with-zmq #:*helper*))

(in-package :zmq)

(load "libzmq.so")
(let ((dir (excl:path-pathname (asdf:system-relative-pathname :acl-zmq ""))))
  (unless (zerop (excl:run-shell-command (format nil "make -C ~a zeromq-thread.so" dir)))
    (error "building zeromq-thread.so failed")))
(load (asdf:system-relative-pathname :acl-zmq "zeromq-thread.so"))

;; Constants

;; Socket types
(defconstant +p2p+ 0)
(defconstant +pub+ 1)
(defconstant +sub+ 2)
(defconstant +req+ 3)
(defconstant +rep+ 4)
(defconstant +xreq+ 5)
(defconstant +xrep+ 6)
(defconstant +upstream+ 7)
(defconstant +downstream+ 8)

;; Sockopts
(defconstant +hwm+ 1)
(defconstant +swap+ 3)
(defconstant +affinity+ 4)
(defconstant +identity+ 5)
(defconstant +subscribe+ 6)
(defconstant +unsubscribe+ 7)
(defconstant +rate+ 8)
(defconstant +recovery-ivl+ 9)
(defconstant +mcast-loop+ 10)
(defconstant +sndbuf+ 11)
(defconstant +rcvbuf+ 12)
(defconstant +rcvmore+ 13)

;; Flags
(defconstant +noblock+ 1)
(defconstant +sndmore+ 2)

;; Poll types
(defconstant +pollin+ 1)
(defconstant +pollout+ 2)
(defconstant +pollerr+ 4)

;; 0MQ errors.

(let ((base 156384712))
  (defconstant +emthread+ (+ base 50))
  (defconstant +efsm+ (+ base 51))
  (defconstant +enocompatproto+ (+ base 52)))

(foreign-functions:def-foreign-call (strerror "zmq_strerror")
    ((errnum :int)) :returning ((* :char)))

(define-condition zmq-error (simple-error)
  ((code :initarg :code :reader zmq-error-code)))
(define-condition zmq-again (zmq-error) ())
(defun zmq-error (code)
  (error (if (eq code excl::*eagain*) 'zmq-again 'zmq-error)
         :format-control (strerror code) :code code))

;; Message type

(defconstant +delimiter+ 31)
(defconstant +vsm+ 32)
(defconstant +msg-more+ 1)
(defconstant +msg-shared+ 128)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-vsm-size+ 30))

(foreign-functions:def-foreign-type msg
    (:struct (content (* :void))
             (shared :unsigned-char)
             (vsm-size :unsigned-char)
             (vsm-data (:array :unsigned-char #.+max-vsm-size+))))

(foreign-functions:def-foreign-call (msg-init "zmq_msg_init")
    ((msg msg)) :returning (:int) :pass-structs-by-value nil)
(foreign-functions:def-foreign-call (msg-init-size "zmq_msg_init_size")
    ((msg msg) (size :long)) :returning (:int) :pass-structs-by-value nil)
(foreign-functions:def-foreign-call (msg-data "zmq_msg_data")
    ((msg msg)) :returning ((* :void)) :pass-structs-by-value nil)
(foreign-functions:def-foreign-call (msg-size "zmq_msg_size")
    ((msg msg)) :returning (:int) :pass-structs-by-value nil)

(foreign-functions:def-foreign-call (msg-close "zmq_msg_close")
    ((msg msg)) :returning (:int) :pass-structs-by-value nil)
(foreign-functions:def-foreign-call (msg-move "zmq_msg_move")
    ((dest msg) (src msg)) :returning (:int) :pass-structs-by-value nil)
(foreign-functions:def-foreign-call (msg-copy "zmq_msg_copy")
    ((dest msg) (src msg)) :returning (:int) :pass-structs-by-value nil)

(foreign-functions:def-foreign-call (memcpy "memcpy")
    ((dst (* :void)) (src (* :void)) (len :long)) :returning ((* :void)))

(defun msg-init-string (msg string)
  (excl:with-native-string (str string :native-length-var len)
    (msg-init-size msg len)
    (memcpy (msg-data msg) str len)))
(defun msg-init-vector (msg vec)
  (let ((len (length vec)))
    (msg-init-size msg len)
    (memcpy (msg-data msg)
            (+ (excl:lispval-to-address vec)
               #.(sys::mdparam 'comp::md-lvector-data0-norm))
            len)))

(defmacro with-msg ((&rest vars) &body body)
  `(let ,(loop :for var :in vars :collect `(,var (ff:allocate-fobject 'msg :c)))
     ,@(loop :for var :in vars :collect `(msg-init ,var))
     (unwind-protect (progn ,@body)
       ,@(loop :for var :in vars :append
            `((msg-close ,var)
              (ff:free-fobject ,var))))))

;; TODO zero-copy
(defun msg-string (msg)
  (excl:native-to-string (msg-data msg) :length (msg-size msg)))
(defun msg-vector (msg)
  (let* ((len (msg-size msg))
         (vec (make-array len :element-type '(unsigned-byte 8))))
    (memcpy (+ (excl:lispval-to-address vec)
               #.(sys::mdparam 'comp::md-lvector-data0-norm))
            (msg-data msg) len)
    vec))

;; C companion thread (see zeromq-thread.c)

(ff:def-foreign-type sem
    (:union
     (nil (:array :unsigned-char 32))
     (align :fixnum)))

(ff:def-foreign-type thread-struct
    (:struct
     (arg-socket (* :void))
     (arg-msg (* msg))
     (arg-string (* :char))
     (arg-len :unsigned-long)
     (arg-int :int)
     (arg-command :int)

     (signal-fd :int)
     (pad :int)
     (context (* :void))
     (sem sem)
     (pthread :long)))

(ff:def-foreign-call (sem_post "sem_post")
    ((sem (* :void)))
  :error-value :errno)

(defun sem-up (sem)
  (multiple-value-bind (res err) (sem_post sem)
    (if (not (zerop res))
	(excl::perror err "sem_post"))))

(defmacro struct-slot (val &rest slot)
  `(ff:fslot-value-typed 'thread-struct :c ,val ,@slot))
(defmacro struct-addr (val &rest slot)
  `(ff:fslot-address-typed 'thread-struct :c ,val ,@slot))

(foreign-functions:def-foreign-call (zmq-thread-init "zmq_thread_init")
    ((context (* :void)) (signal-fd :int)) :returning ((* :void)))

(foreign-functions:def-foreign-call (zmq-init "zmq_init")
    ((io-threads :int)) :returning ((* :void)) :error-value :errno)
(foreign-functions:def-foreign-call (zmq-term "zmq_term")
    ((context (* :void))) :returning (:int))

(defstruct zmq-context
  (lock (mp:make-process-lock))
  context)

(defstruct zmq-thread
  input
  output
  struct)

(defvar *context* (make-zmq-context))
(defvar *helper*)

(defun start-helper ()
  (mp:with-process-lock ((zmq-context-lock *context*))
    (unless (zmq-context-context *context*)
      (multiple-value-bind (context errno) (zmq-init 1)
        (when (eq context 0) (zmq-error errno))
        (setf (zmq-context-context *context*) context))))
  (multiple-value-bind (in out) (excl.osi:pipe)
    (let ((struct (zmq-thread-init (zmq-context-context *context*) (excl:stream-output-fn out))))
      (make-zmq-thread :input in :output out :struct struct))))

(defun close-helper (thread)
  (setf (struct-slot (zmq-thread-struct thread) 'arg-command) 0)
  (sem-up (struct-addr (zmq-thread-struct thread) 'sem))
  (close (zmq-thread-input thread)))

(defmacro with-zmq (&body body)
  (let ((already-bound (gensym)))
    `(let* ((,already-bound (boundp '*helper*))
            (*helper* (if ,already-bound *helper* (start-helper))))
       (unwind-protect (progn ,@body)
         (unless ,already-bound
           (close-helper *helper*))))))

;; Access through companion thread

(defmacro zmq-thread-access (code returns errval &body args)
  (flet ((arg (name) (intern (format nil "~a~a" :arg- name))))
    (let ((struct (gensym)))
      `(let ((,struct (zmq-thread-struct *helper*)))
         ,@(loop :for (field val) :in args :collect
              `(setf (struct-slot ,struct ',(arg field)) ,val))
         (setf (struct-slot ,struct 'arg-command) ,code)
         (sem-up (struct-addr ,struct 'sem))
         (assert (eql (read-byte (zmq-thread-input *helper*))
                      #.(char-code #\K)))
         (let ((ret (struct-slot ,struct ',(arg returns))))
           (when (eq ret ,errval) (zmq-error (struct-slot ,struct 'arg-command)))
           ret)))))

(defun socket (type)
  (zmq-thread-access 1 socket 0
    (int type)))
(defun socket-close (socket)
  (zmq-thread-access 2 int -1
    (socket socket)))

(defmacro with-socket ((var type) &body body)
  `(let ((,var (socket ,type)))
     (unwind-protect (progn ,@body) (socket-close ,var))))

(defun bind (socket string)
  (zmq-thread-access 3 int -1
    (socket socket)
    (string (excl:string-to-native string))))
(defun connect (socket string)
  (zmq-thread-access 4 int -1
    (socket socket)
    (string (excl:string-to-native string))))

(defun getsockopt-str (socket option max-length)
  (let ((buf (excl:aclmalloc max-length)))
    (unwind-protect
         (progn
           (zmq-thread-access 5 int -1
             (socket socket)
             (int option)
             (string buf)
             (len max-length))
           (excl:native-to-string buf :length (struct-slot (zmq-thread-struct *helper*) 'arg-len)))
      (excl:aclfree buf))))
(defun getsockopt-num (socket option)
  (ff:with-static-fobjects ((opt :long))
    (zmq-thread-access 5 int -1
      (socket socket)
      (int option)
      (string (ff:fslot-address opt))
      (len #.(ff:sizeof-fobject :long)))
    (ff:fslot-value opt)))

(defun setsockopt-str (socket option value &optional length)
  (zmq-thread-access 6 int -1
    (socket socket)
    (int option)
    (string (excl:string-to-native value))
    (len (or length (length value)))))
(defun setsockopt-num (socket option value)
  (ff:with-static-fobjects ((opt :long))
    (setf (ff:fslot-value opt) value)
    (zmq-thread-access 6 int -1
      (socket socket)
      (int option)
      (string (ff:fslot-address opt))
      (len #.(ff:sizeof-fobject :long)))))

(defun send (socket msg &optional (flags 0))
  (zmq-thread-access 7 int -1
    (socket socket)
    (msg msg)
    (int flags)))
(defun recv (socket msg &optional (flags 0))
  (zmq-thread-access 8 int -1
    (socket socket)
    (msg msg)
    (int flags)))

(defun do-poll (items nitems timeout)
  (zmq-thread-access 9 int -1
    (socket items)
    (int nitems)
    (len timeout)))

;; Poll wrapper

(foreign-functions:def-foreign-type pollitem
    (:struct (socket (* :void))
             (fd :int)
             (events :short)
             (revents :short)))

(defun init-pollitems (pollitems itemvar handlervar)
  `(setf ,@(loop :for item :in pollitems :for index :from 0 :append
              (destructuring-bind ((&key socket fd event) body) item
                `((ff:fslot-value-typed '(:array pollitem) :c ,itemvar ,index 'socket) ,(or socket 0)
                  (ff:fslot-value-typed '(:array pollitem) :c ,itemvar ,index 'fd) ,(or fd -1)
                  (ff:fslot-value-typed '(:array pollitem) :c ,itemvar ,index 'events) ,(or event +pollin+)
                  (aref ,handlervar ,index) ,body)))))

(defun parse-poll-clauses (clauses)
  (let (pollitems timeout)
    (dolist (clause clauses)
      (destructuring-bind ((directive &optional arg (event :pollin)) &body body) clause
        (ecase directive
          (:fd (push `((:fd ,arg :event ,event) ,(gensym) ,body) pollitems))
          (:socket (push `((:socket ,arg :event ,event) (lambda () ,@body)) pollitems))
          (:timeout (when timeout (error "duplicate :timeout clause in do-polling"))
                    (setf timeout (list arg `(lambda () ,@body)))))))
    (values (nreverse pollitems) timeout)))

(defmacro poll (&body clauses)
  (multiple-value-bind (pollitems timeout) (parse-poll-clauses clauses)
    (let ((nitems (length pollitems))
          (handlers (gensym)) (items (gensym)) (timeoutfunc (gensym)))
      `(block nil
         (let ((,handlers (make-array ,nitems))
               ,@(if timeout `((,timeoutfunc ,(second timeout)))))
           (ff:with-static-fobject (,items `(:array pollitem ,,nitems) :allocation :c)
             ,(init-pollitems pollitems items handlers)
             (loop
                (let ((count (do-poll ,items ,nitems ,(if timeout (first timeout) -1))))
                  (if (plusp count)
                      (dotimes (i ,nitems)
                        (when (plusp (ff:fslot-value-typed '(:array pollitem) :c ,items i 'revents))
                          (funcall (aref ,handlers i))
                          (when (zerop (decf count)) (return))))
                      ,@(when timeout `((funcall ,timeoutfunc))))))))))))
