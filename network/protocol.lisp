(in-package :cl-bodge.network)

;;
(defgeneric decode-value (chunk stream))
(defgeneric next-chunk (chunk))
(defgeneric encode-value (chunk value stream))


(defclass data-chunk ()
  ((size :initform 0 :initarg :size :type fixnum :reader size-of)))


;;
(defclass message-chunk (data-chunk) ())


(defmethod decode-value ((this message-chunk) stream)
  (decode-message stream))


(defstruct (encoded-message
             (:constructor make-encoded-message (buffer start end)))
  (buffer (make-array 0 :element-type '(unsigned-byte 8))
          :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (start 0 :read-only t)
  (end 0 :read-only t))


(defmethod encode-value ((this message-chunk) (value encoded-message) stream)
  (write-sequence (encoded-message-buffer value) stream
                  :start (encoded-message-start value)
                  :end (encoded-message-end value)))


(defmethod next-chunk ((this message-chunk))
  (make-instance 'message-size-chunk))

;;
(defclass message-size-chunk (data-chunk)
  ((message-size :initform 0))
  (:default-initargs :size 2))


(defmethod decode-value ((this message-size-chunk) stream)
  (with-slots (message-size) this
    (let ((stream (flex:make-flexi-stream stream :element-type '(unsigned-byte 16))))
      (setf message-size (read-byte stream)))))


(defmethod encode-value ((this message-size-chunk) (value fixnum) stream)
  (check-type value (unsigned-byte 16))
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))


(defmethod next-chunk ((this message-size-chunk))
  (with-slots (message-size) this
    (make-instance 'message-chunk :size message-size)))

;;
(defclass protocol-version-chunk (data-chunk) ()
  (:default-initargs :size 1))


(defmethod decode-value ((this protocol-version-chunk) stream)
  (read-byte stream))


(defmethod encode-value ((this protocol-version-chunk) (value fixnum) stream)
  (check-type value (unsigned-byte 8))
  (write-byte value stream))


(defmethod next-chunk ((this protocol-version-chunk))
  (make-instance 'message-size-chunk))

;;
(defclass magic-chunk (data-chunk) ()
  (:default-initargs :size (length +protocol-magic+)))


(defmethod decode-value ((this magic-chunk) stream)
  (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence result stream)
    result))


(defmethod encode-value ((this magic-chunk) (value array) stream)
  (check-type value (array (unsigned-byte 8) (4)))
  (write-sequence value stream))


(defmethod next-chunk ((this magic-chunk))
  (make-instance 'protocol-version-chunk))
