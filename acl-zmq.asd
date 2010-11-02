(defsystem :acl-zmq
  :depends-on (:shared-mem)
  :components ((:file "zeromq")))

(unless (zerop (excl:run-shell-command (format nil "make -C ~a zeromq-thread.so" 
                                               (asdf:system-relative-pathname :acl-zmq ""))))
  (error "make zeromq-thread.so failed"))
