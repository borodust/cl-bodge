(cl:in-package :cl-bodge.resources)


;;;
;;;  Resource handler
;;;
(defgeneric encode-resource (handler resource stream)
  (:method (handler resource stream)))


(defgeneric decode-resource (handler stream)
  (:method (handler stream)))


(defgeneric handler-resource-type (handler))


(defgeneric resource-dependencies (handler resource)
  (:method (handler resource) nil))


(defgeneric make-resource-handler (type &key &allow-other-keys))


(defclass resource-handler ()
  ((resource-type :initform (error ":resource-type missing") :initarg :resource-type
                  :reader handler-resource-type)))


;;;
;;; Text resource handler
;;;
(defclass text-resource-handler (resource-handler)
  ((encoding :initarg :encoding :initform (error ":encoding missing")))
  (:default-initargs :resource-type :text))


(defmethod decode-resource ((this text-resource-handler) stream)
  (with-slots (encoding) this
    ;; FIXME: this is suboptimal, but flexi stream doesn't seem to handle
    ;; buffered read very well and throws errors like
    ;;   `Unexpected value #x89 at start of UTF-8 sequence.`
    ;; if you try to read from it in chunks
    ;;
    ;; Need to somehow get back to buffered reading with
    ;;   (flex:make-flexi-stream stream :external-format encoding)
    (flexi-streams:octets-to-string (read-stream-content-into-byte-vector stream)
                                    :external-format encoding)))


(defmethod encode-resource ((this text-resource-handler) (resource string) stream)
  (with-slots (encoding) this
    (let ((string-stream (flex:make-flexi-stream stream :external-format encoding)))
      (write-sequence resource string-stream))))


(defun make-text-resource-handler (&optional encoding)
  (make-instance 'text-resource-handler :encoding (or encoding :utf-8)))


(defmethod make-resource-handler ((type (eql :text)) &key (encoding :utf-8))
  (make-text-resource-handler encoding))


;;;
;;; Binary resource handler
;;;
(defclass binary-resource-handler (resource-handler) ()
  (:default-initargs :resource-type :binary))


(defmethod decode-resource ((this binary-resource-handler) stream)
  (bodge-util:read-stream-content-into-byte-vector stream))


(defmethod encode-resource ((this binary-resource-handler) resource stream)
  (write-sequence resource stream))


(defun make-binary-resource-handler ()
  (make-instance 'binary-resource-handler))


(defmethod make-resource-handler ((type (eql :binary)) &key)
  (make-binary-resource-handler))
