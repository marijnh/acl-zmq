(defpackage :zmq
  (:use :cl)
  (:export #:+p2p+ #:+pub+ #:+sub+ #:+req+ #:+rep+ #:+xreq+ #:+xrep+ #:+upstream+ #:+downstream+

           #:+hwm+ #:+swap+ #:+affinity+ #:+identity+ #:+subscribe+ #:+unsubscribe+ #:+rate+
           #:+recovery-ivl+ #:+mcast-loop+ #:+sndbuf+ #:+rcvbuf+ #:+rcvmore+
           
           #:+noblock+ #:+sndmore+
           #:+pollin+ #:+pollout+ #:+pollerr+

           #:zmq-error #:zmq-error-code #:zmq-again
           
           #:msg #:msg-close #:msg-copy #:with-msg

           #:socket #:socket-close #:bind #:connect
           #:getsocktopt-str #:getsockopt-num #:setsockopt
           #:send #:recv #:poll))

(in-package :zmq)

(load "libzmq.so")
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
  ((code :initarg code :reader zmq-error-code)))
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

(defun msg (&key size data)
  (let ((struct (excl:aclmalloc (ff:sizeof-fobject 'msg))))
    (cond (size (msg-init-size struct size))
          (data (etypecase data
                  (string
                   (excl:with-native-string (str data :native-length-var len)
                     (msg-init-size struct len)
                     (memcpy (msg-data struct) str len)))
                  ((simple-array (unsigned-byte 8))
                   (msg-init-size struct (length data))
                   (memcpy (msg-data struct)
                           (+ (excl:lispval-to-address data) ;; TODO test this
                              #.(sys::mdparam 'comp::md-lvector-data0-norm))
                           (length data)))))
          (t (msg-init struct)))
    struct))

(defmacro with-msg ((var &rest args) &body body)
  `(let ((,var (msg ,@args)))
     (unwind-protect (progn ,@body)
       (msg-close ,var))))

;; C companion thread (see zeromq-thread.c)

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
     (sem ipc.posix:sem)
     (pthread :long)))

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
  context
  (refcount 0))

(defstruct zmq-thread
  input
  output
  struct)

(defvar *context* (make-zmq-context))
(defvar *thread*)

(defun start-zmq-thread ()
  (mp:with-process-lock ((zmq-context-lock *context*))
    (when (eq (zmq-context-refcount *context*) 0)
      (multiple-value-bind (context errno) (zmq-init 1)
        (when (eq context 0) (zmq-error errno))
        (setf (zmq-context-context *context*) context)))
    (incf (zmq-context-refcount *context*)))
  (multiple-value-bind (in out) (excl.osi:pipe)
    (let ((struct (zmq-thread-init (zmq-context-context *context*) (excl:stream-output-fn out))))
      (make-zmq-thread :input in :output out :struct struct))))

(defun close-zmq-thread (thread)
  (mp:with-process-lock ((zmq-context-lock *context*))
    (when (eq (decf (zmq-context-refcount *context*)) 0)
      (zmq-term (zmq-context-context *context*))))
  (setf (struct-slot (zmq-thread-struct thread) 'arg-command) 0)
  (ipc.posix:sem-up (struct-addr (zmq-thread-struct thread) 'sem))
  (close (zmq-thread-input thread)))

(defmacro with-zmq (&body body)
  `(let ((*thread* (start-zmq-thread)))
    (unwind-protect (progn ,@body)
      (close-zmq-thread *thread*))))

;; Access through companion thread

(defmacro zmq-thread-access (code returns errval &body args)
  (flet ((arg (name) (intern (format nil "~a~a" :arg- name))))
    (let ((struct (gensym)))
      `(let ((,struct (zmq-thread-struct *thread*)))
         ,@(loop :for (field val) :in args :collect
              `(setf (struct-slot ,struct ',(arg field)) ,val))
         (setf (struct-slot ,struct 'arg-command) ,code)
         (ipc.posix:sem-up (struct-addr ,struct 'sem))
         (assert (eql (read-char (zmq-thread-input *thread*)) #\K))
         (let ((ret (struct-slot ,struct ',(arg returns))))
           (when (eq ret ,errval) (zmq-error (struct-slot ,struct 'arg-command)))
           ret)))))

(defun socket (type)
  (zmq-thread-access 1 socket 0
    (int type)))
(defun socket-close (socket)
  (zmq-thread-access 2 int -1
    (socket socket)))
  
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
           (excl:native-to-string buf :length (struct-slot (zmq-thread-struct *thread*) 'arg-len)))
      (excl:aclfree buf))))
(defun getsockopt-num (socket option)
  (ff:with-static-fobjects ((opt :long))
    (zmq-thread-access 5 int -1
      (socket socket)
      (int option)
      (string (ff:fslot-address opt))
      (len (ff:sizeof-fobject :long)))
    (ff:fslot-value opt)))

(defun setsockopt-str (socket option value &optional length)
  (zmq-thread-access 6 int -1
    (socket socket)
    (int option)
    (string (excl:string-to-native value)) ;; TODO other types
    (len (or length (length value)))))

(defun send (socket msg flags)
  (zmq-thread-access 7 int -1
    (socket socket)
    (msg msg)
    (int flags)))
(defun recv (socket msg flags)
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
                `((ff:fslot-value-typed 'pollitem :static ,itemvar ,index 'socket) ,(or socket 0)
                  (ff:fslot-value-typed 'pollitem :static ,itemvar ,index 'fd) ,(or fd -1)
                  (ff:fslot-value-typed 'pollitem :static ,itemvar ,index 'events) ,(or event +pollin+)
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
    (list (nreverse pollitems) timeout)))

(defmacro poll (&body clauses)
  (destructuring-bind (pollitems timeout) (parse-poll-clauses clauses)
    (let ((nitems (length pollitems))
          (handlers (gensym)) (items (gensym)) (timeoutfunc (gensym)))
      `(block nil
         (let ((,handlers (make-array ,nitems))
               ,@(if timeout `((,timeoutfunc ,(second timeout)))))
           (ff:with-static-fobject (,items `(:array pollitem ,,nitems))
             ,@(init-pollitems pollitems items handlers)
             (loop
                (let ((count (do-poll ,items ,nitems ,(if timeout (first timeout) -1))))
                  (if (plusp count)
                      (dotimes (i ,nitems)
                        (when (plusp (ff:fslot-value-typed 'pollitem :static ,items i 'revents))
                          (funcall (aref ,handlers i))
                          (when (zerop (decf count)) (return))))
                      ,@(when timeout `((funcall ,timeoutfunc))))))))))))
