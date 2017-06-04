(in-package :cl-bodge.network)

(define-constant +transfer-buffer-size+ (* 64 1024))


(define-condition premature-frame-end (simple-error)
  ((source :initform nil :initarg :source)))

;;
(defgeneric decode-frame (decoder stream))
(defgeneric encode-frame (encoder))


(defmethod decode-frame :around (decoder stream)
  (flet ((%throw-premature-end (e)
           (error (make-condition 'premature-frame-end :source e))))
    (handler-bind ((end-of-file #'%throw-premature-end))
      (call-next-method))))


;;;
;;; Frames
;;;
(defclass frame ()
  ((body :initform nil :initarg :body :reader body-of)))


(defclass length-field-frame-decoder () ())


(definline make-length-field-frame-decoder ()
  (make-instance 'length-field-frame-decoder))


(defclass length-field-frame-encoder ()
  ((buffered-output-stream :initform (make-buffered-output-stream
                                      (make-array +max-frame-size+
                                                  :element-type '(unsigned-byte 8))))))


(definline make-length-field-frame-encoder ()
  (make-instance 'length-field-frame-encoder))


(defclass bounded-frame (frame)
  ((size :initarg :size :type fixnum :reader size-of)))


(defmethod decode-frame ((decoder length-field-frame-decoder) stream)
  (let ((body-size 0))
    (flet ((%read-byte-into (pos)
             (setf (ldb (byte 8 pos) body-size) (read-byte stream))))
      (%read-byte-into 0)
      (%read-byte-into 8)
      (%read-byte-into 16)
      (%read-byte-into 24))
    (let ((bounded-stream (flex:make-flexi-stream stream
                                                  :element-type '(unsigned-byte 8)
                                                  :position 0
                                                  :bound body-size)))
      (make-instance 'bounded-frame
                     :size body-size
                     :body bounded-stream))))


(defmethod encode-frame ((encoder length-field-frame-encoder))
  (with-slots (buffered-output-stream) encoder
    (lambda (body-encoder stream)
      (file-position buffered-output-stream 0)
      (funcall body-encoder buffered-output-stream)
      (let ((len (file-position buffered-output-stream)))
        (write-byte (ldb (byte 8 0) len) stream)
        (write-byte (ldb (byte 8 8) len) stream)
        (write-byte (ldb (byte 8 16) len) stream)
        (write-byte (ldb (byte 8 24) len) stream)
        (write-sequence (buffer-of buffered-output-stream) stream :start 0 :end len)))))


;;;
;;; Pipelines
;;;
(defclass decoding-pipeline ()
  ((stream-buffer :initform (make-circular-buffer (* 2 +max-frame-size+)))
   (frame-buffer :initform (make-array +max-frame-size+
                                       :element-type '(unsigned-byte 8)))
   (frame-buffer-end :initform 0)
   (pipeline :initform nil :initarg :decoders)))


(definline make-decoding-pipeline (decoders)
  (make-instance 'decoding-pipeline :decoders decoders))


(defun decode-inbound (decoding-pipeline stream)
  (with-slots (stream-buffer frame-buffer frame-buffer-end pipeline)
      decoding-pipeline
    (when pipeline
      (fill-buffer stream-buffer stream)
      (let ((bytes-read (read-buffer stream-buffer frame-buffer :start frame-buffer-end)))
        (incf frame-buffer-end bytes-read))
      (flex:with-input-from-sequence (stream frame-buffer :end frame-buffer-end)
        (block reading
          (handler-bind ((premature-frame-end (lambda (e)
                                                (declare (ignore e))
                                                (return-from reading))))
            (loop with frame = (decode-frame (first pipeline) stream)
               for decoder in (rest pipeline)
               do (setf frame (decode-frame decoder frame))
               finally
                 (setf frame-buffer-end 0)
                 (return frame))))))))


(defclass encoding-pipeline ()
  ((stream-buffer :initform (make-circular-buffer +max-frame-size+))
   (pipeline :initform nil :initarg :encoders)))


(definline make-encoding-pipeline (encoders)
  (make-instance 'encoding-pipeline :encoders encoders))


(defun %encode (encoder-list stream)
  (when encoder-list
    (flet ((body-encoder (stream)
             (%encode (rest encoder-list) stream)))
      (funcall (first encoder-list) #'body-encoder stream))))


(defun encode-outbound (encoding-pipeline frame stream)
  (with-slots (pipeline) encoding-pipeline
    (when pipeline
      (flet ((to-encoding-fu (encoder)
               (encode-frame encoder))
             (return-frame (encoder stream)
               (declare (ignore encoder stream))
               frame))
        (let ((encoding-fu-list (mapcar #'to-encoding-fu pipeline)))
          (%encode (nconc encoding-fu-list (list #'return-frame)) stream))))))
