(in-package :cl-bodge.network)


(defgeneric receive-message (channel message)
  (:method (channel message)))


(defgeneric send-message (channel message))


(defmethod receive-message :around (channel message)
  (handler-case
      (call-next-method)
    (serious-condition (e)
      (log:error "Unhandled error during message processing:~%~A: ~A" (type-of e) e))))


;;
(defclass channel (lockable disposable)
  ((stream :initform (error ":stream missing") :initarg :stream :reader stream-of)
   (sink :initform nil)
   (buffered-output-stream :initform (make-buffered-output-stream
                                      (make-array +max-frame-size+
                                                  :element-type '(unsigned-byte 8))))
   (encoding-pipeline)
   (decoding-pipeline)))


(define-destructor channel (stream)
  (close stream))


(defmethod decode-frame ((this channel) (frame frame))
  (with-slots (buffered-output-stream stream) this
    (let ((message (decode-message (body-of frame))))
      (run (concurrently ()
             (receive-message this message))))))


(defmethod encode-frame ((this channel))
  (lambda (body-encoder stream)
    (let ((message (funcall body-encoder stream)))
      (encode-message message stream))))


(defmethod send-message ((this channel) message)
  (with-slots (stream encoding-pipeline) this
    (flet ((%send ()
             (encode-outbound encoding-pipeline message stream)))
      (dispatch (network) #'%send nil))))


(definline acknowledge (message channel)
  (send-message channel (make-message 'ack-message :id (message-id message))))


(defun drain-channel (channel)
  (with-slots (stream decoding-pipeline) channel
    (decode-inbound decoding-pipeline stream)))


(defmethod initialize-instance :after ((this channel) &key)
  (with-slots (stream message-table encoding-pipeline decoding-pipeline) this
    (let ((decoders (list (make-length-field-frame-decoder)
                          this))
          (encoders (list (make-length-field-frame-encoder)
                          this)))
      (setf encoding-pipeline (make-encoding-pipeline encoders)
            decoding-pipeline (make-decoding-pipeline decoders)))))


;;;
;;; Message tracking and dispatching channel
;;;

(defclass dispatching-channel (channel)
  ((message-table :initform (make-hash-table :test 'eql))))


(defun %pop-handler (channel reply-id)
  (with-slots (message-table) channel
    (with-instance-lock-held (channel)
      (let ((handler (gethash reply-id message-table)))
        (when handler
          (remhash reply-id message-table))
        handler))))


(defmethod receive-message ((this dispatching-channel) message)
  (with-slots (message-table) this
    (if-let ((reply-id (reply-id message)))
      (if-let ((handler (%pop-handler this reply-id)))
        (run (concurrently ()
               (funcall handler message)))
        (log:warn "Handler not found for message with id ~A" reply-id))
      (call-next-method))))


(defun %dispatch (channel message response-callback)
  (with-slots (message-table) channel
    (if-let ((message-id (message-id message)))
      (progn
        (with-instance-lock-held (channel)
          (setf (gethash message-id message-table) response-callback))
        (send-message channel message))
      (progn
        (send-message channel message)
        (funcall response-callback nil)))))


(defun message-flow (channel message)
  (%> () (%dispatch channel message #'continue-flow)))
