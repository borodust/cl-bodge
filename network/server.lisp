(in-package :cl-bodge.network)


(define-constant +default-connection-timeout+ (* 5 60))


(declaim (special *server*))


(defclass server (disposable dispatching)
  ((conduit-table :initform (make-hash-table))
   (server)))


(define-destructor server (server)
  (run (-> ((network)) ()
         (as:close-tcp-server server))))


(defgeneric process-condition (server condition)
  (:method (server condition)
    (log:error "Unhandled event ~A: ~A" (type-of condition) condition)))


(defmethod process-condition ((this server) (condi as:socket-error))
  (with-slots (conduit-table) this
    (let ((socket (as:socket condi)))
      (dispose (gethash socket conduit-table))
      (as:close-socket socket)
      (remhash socket conduit-table))))


(defmethod initialization-flow ((this server)
                                &key (host nil)
                                  (port (error ":port missing"))
                                  (connection-timeout +default-connection-timeout+)
                                  (conduit-constructor (error ":conduit-constructor missing")))
  (with-slots (server conduit-table) this
    (labels ((on-accept (socket)
               (as:set-socket-timeouts socket connection-timeout nil)
               (flet ((return-early (e)
                        (log:error "Error during peer registration: ~a"e)
                        (unwind-protect
                             (as:close-socket socket)
                          (return-from on-accept))))
                 (let ((conduit (handler-bind ((serious-condition #'return-early))
                                  (funcall conduit-constructor
                                           (make-instance 'as:async-io-stream
                                                          :socket socket)))))
                   (setf (gethash socket conduit-table) conduit))))
             (process-input (socket stream)
               (declare (ignore stream))
               (log-errors
                 (drain-stream (gethash socket conduit-table))))
             (process-event (event)
               (process-condition this event))
             (%make-server (host port)
               (as:tcp-server host port #'process-input
                              :event-cb #'process-event
                              :connect-cb #'on-accept
                              :stream t)))
      (>> (call-next-method)
          (-> ((network)) ()
            (setf server (%make-server host port)))))))


(defun accept-flow (host port conduit-constructor
                    &key (connection-timeout +default-connection-timeout+))
  (assembly-flow 'server
                 :host host :port port
                 :connection-timeout connection-timeout
                 :conduit-constructor conduit-constructor))
