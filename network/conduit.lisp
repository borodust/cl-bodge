(in-package :cl-bodge.network)


(declaim (special *message*
                  *conduit*))


(defgeneric receive-message (receiver message))

(defmethod receive-message :around (receiver message)
  (handler-case
      (call-next-method)
    (serious-condition (e)
      (log:error "Unhandled error during message processing:~%~A: ~A" (type-of e) e))))

;;
(defclass conduit (lockable dispatching disposable)
  ((stream :initform (error ":stream missing") :initarg :stream :reader stream-of)
   (sink :initform nil)
   (buffered-output-stream :initform (make-buffered-output-stream
                                      (make-array +max-data-chunk-size+
                                                  :element-type '(unsigned-byte 8))))
   (message-table :initform (make-hash-table :test 'eql))))


(define-destructor conduit (stream)
  (close stream))


(defgeneric process-chunk (conduit chunk value)
  (:method (conduit chunk value)
    (declare (ignore conduit chunk value))))


(defmethod process-chunk ((this conduit) (chunk magic-chunk) value)
  (unless (equalp value +protocol-magic+)
    (error "Unsupported protocol")))


(defmethod process-chunk ((this conduit) (chunk protocol-version-chunk) value)
  (unless (equal value +protocol-version+)
    (error "Unsupported protocol")))


(defun %send-message (conduit message)
  (with-slots (buffered-output-stream stream) conduit
    (file-position buffered-output-stream 0)
    (encode-message message buffered-output-stream)
    (let ((message-size (file-position buffered-output-stream)))
      (encode-value (make-instance 'message-size-chunk)
                    message-size
                    stream)
      (encode-value (make-instance 'message-chunk)
                    (make-encoded-message (buffer-of buffered-output-stream) 0 message-size)
                    stream)
      (force-output stream))))


(defgeneric send-message (conduit message))


(defmethod send-message ((this conduit) (message message))
  (flet ((%send ()
           (%send-message this message)))
    (dispatch (network) #'%send nil)))


(definline acknowledge (message conduit)
  (send-message conduit (make-message 'ack-message :for-id (message-id message))))


(defmethod process-chunk ((this conduit) (chunk message-chunk) message)
  (with-slots (message-table buffered-output-stream stream) this
    (let* ((*conduit* this))
      (if (typep message 'reply-message)
          (let ((reply-id (reply-for-id message)))
            (with-instance-lock-held (this)
              (if-let ((handler (gethash reply-id message-table)))
                (progn
                  (remhash reply-id message-table)
                  (funcall handler message))
                (log:error "Handler not found for message with id ~A" reply-id))))
          (run (concurrently ()
                 (receive-message this message)))))))


(defun drain-stream (conduit)
  (with-slots (stream sink) conduit
    (flet ((%drain ()
             (process-chunks sink stream)))
      (dispatch (network) #'%drain nil))))


(defmethod initialize-instance :after ((this conduit) &key)
  (with-slots (stream message-table sink) this
    (flet ((on-chunk-read (chunk value)
             (process-chunk this chunk value)))
      (setf sink (make-instance 'chunk-sink
                                :on-chunk-read #'on-chunk-read
                                :chunk (make-instance 'magic-chunk)))
      (encode-value (make-instance 'magic-chunk) +protocol-magic+ stream)
      (encode-value (make-instance 'protocol-version-chunk) +protocol-version+ stream))))


(defmethod dispatch ((this conduit) (task function) invariant &key message)
  (declare (ignore invariant))
  (with-slots (message-table) this
    (with-instance-lock-held (this)
      (flet ((response-callback (message)
               (let ((*message* message))
                 (funcall task))))
        (if-let ((message-id (message-id message)))
          (progn
            (setf (gethash message-id message-table) #'response-callback)
            (send-message this message))
          (progn
            (send-message this message)
            (response-callback nil)))))))
