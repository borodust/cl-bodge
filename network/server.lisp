(in-package :cl-bodge.network)


(define-constant +default-connection-timeout+ (* 5 60))


(declaim (special *server*))


(defclass server (disposable dispatching)
  ((channel-table :initform (make-hash-table))
   (server-socket)))


(define-destructor server (server-socket)
  (run (-> ((network)) ()
         (dispose server-socket))))


(defgeneric process-condition (server condition)
  (:method (server condition)
    (log:error "Unhandled event ~A: ~A" (type-of condition) condition)))


(defmethod initialization-flow ((this server)
                                &key (host "0.0.0.0")
                                  (port (error ":port missing"))
                                  (connection-timeout +default-connection-timeout+)
                                  (channel-constructor (error ":channel-constructor missing")))
  (with-slots (server-socket channel-table) this
    (labels ((%drain-stream (stream)
               (drain-channel (gethash (socket-of stream) channel-table)))
             (%register-client (socket)
               (setf (gethash socket channel-table)
                     (funcall channel-constructor
                              (make-socket-stream socket :on-read #'%drain-stream)))))
    (>> (call-next-method)
        (-> ((network)) ()
          (setf server-socket (make-socket :on-connect #'%register-client))
          (socket-accept server-socket host port))))))


(defun accept-flow (port channel-constructor
                    &key (host "0.0.0.0")
                      (connection-timeout +default-connection-timeout+))
  (assembly-flow 'server
                 :host host :port port
                 :connection-timeout connection-timeout
                 :channel-constructor channel-constructor))
