(defpackage :acl-zmq-test
  (:use :cl))

(in-package :acl-zmq-test)

(defun pub-thread ()
  (zmq:with-zmq
    (zmq:with-msg (msg)
      (zmq:with-socket (sock zmq:+pub+)
        (zmq:bind sock "tcp://*:11331")
        (dotimes (i 30)
          (sleep .1)
          (zmq:with-msg-init (msg :string "hello there")
            (zmq:send sock msg)))))))

(defun sub-test ()
  (zmq:with-zmq
    (zmq:with-msg (msg)
      (zmq:with-socket (sock zmq:+sub+)
        (zmq:connect sock "tcp://localhost:11331")
        (zmq:setsockopt-str sock zmq:+subscribe+ "")
        (mp:process-run-function "pub" 'pub-thread)
        (let ((start-time (get-internal-real-time))
              (saw-msg 0))
          (zmq:poll
            ((:socket sock)
             (zmq:recv sock msg)
             (assert (equal (zmq:msg-string msg) "hello there"))
             (incf saw-msg))
            ((:timeout 100000)
             (when (> (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second) 1)
               (return))))
          (assert (> saw-msg 0)))))))
