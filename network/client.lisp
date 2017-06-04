(in-package :cl-bodge.network)


;;
(defclass client (disposable)
  ((channel :reader channel-of)
   (connection)))


(defmethod initialization-flow ((this client) &key host port channel-constructor)
  (with-slots (channel connection) this
    (>> (call-next-method)
        (%> ()
          (labels ((%drain-stream (stream)
                     (declare (ignore stream))
                     (drain-channel channel))
                   (%register-channel (socket)
                     (setf channel
                           (funcall channel-constructor
                                    (make-socket-stream socket :on-read #'%drain-stream)))
                     (continue-flow)))
            (run (-> ((network)) ()
                   (setf connection (make-connection :on-connect #'%register-channel))
                   (connect connection host port))))))))


(defun connect-flow (host port channel-constructor)
  (assembly-flow 'client
                 :host host
                 :port port
                 :channel-constructor channel-constructor))


(define-destructor client (connection)
  (run (-> ((network)) ()
         (dispose connection))))
