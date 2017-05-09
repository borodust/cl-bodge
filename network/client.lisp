(in-package :cl-bodge.network)


;;
(defclass client (disposable)
  ((conduit :reader conduit-of)))


(defmethod initialization-flow ((this client) &key host port conduit-constructor)
  (with-slots (conduit) this
    (flet ((read-incoming (socket stream)
             (declare (ignore socket stream))
             (drain-stream conduit))
           (process-event (ev)
             (log:warn ev)))
      (>> (call-next-method)
          (-> ((network)) ()
              (let ((stream (as:tcp-connect host port #'read-incoming
                                            :stream t
                                            :event-cb #'process-event)))
                (setf conduit (funcall conduit-constructor stream))))))))


(defun connect-flow (host port conduit-constructor)
  (assembly-flow 'client
                 :host host
                 :port port
                 :conduit-constructor conduit-constructor))


(define-destructor client (conduit)
  (close (stream-of conduit)))
