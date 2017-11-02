(in-package :cl-bodge.resources)


;;;
;;;  Resource handler
;;;

(defgeneric encode-resource (handler resource stream)
  (:method (handler resource stream)))


(defgeneric decode-resource (handler stream)
  (:method (handler stream)))


;;;
;;; Text resource handler
;;;
(defclass text-resource-handler ()
  ((encoding :initarg :encoding :initform (error ":encoding missing"))))


(defmethod decode-resource ((this text-resource-handler) stream)
  (with-slots (encoding) this
    (let ((string-stream (flex:make-flexi-stream stream :external-format encoding)))
      (read-stream-content-into-string string-stream))))


(defmethod encode-resource ((this text-resource-handler) (resource string) stream)
  (with-slots (encoding) this
    (let ((string-stream (flex:make-flexi-stream stream :external-format encoding)))
      (write-sequence resource string-stream))))


(defun make-text-resource-handler (&optional (encoding :utf-8))
  (make-instance 'text-resource-handler :encoding encoding))
