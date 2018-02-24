(cl:in-package :cl-bodge.resources)


(defstruct (image-chunk
             (:constructor make-image-chunk (name width height pixel-format data)))
  (name nil :read-only t)
  (width nil :read-only t)
  (height nil :read-only t)
  (pixel-format nil :read-only t)
  (data nil :read-only t))


(defmethod read-chunk ((type (eql :image)) parameters stream)
  (destructuring-bind (&key size &allow-other-keys) parameters
    (let* ((image-data (make-array size :element-type '(unsigned-byte 8)))
           (bytes-read (read-sequence image-data stream)))
      (unless (= size bytes-read)
        (error "Incorrect :size provided for image chunk data: ~a supplied, but ~a read"
               size bytes-read))
      (parse-chunk type parameters image-data))))


(defmethod parse-chunk ((type (eql :image)) parameters data)
  (destructuring-bind (&key name width height type pixel-format &allow-other-keys) parameters
    (unless (eq type :raw)
      (error "Image type ~a unsupported" type))
    (unless (pixel-format-p pixel-format)
      (error "Unsupported pixel format: ~a" pixel-format))
    (make-image-chunk name width height pixel-format data)))
