(in-package :cl-bodge.graphics)

(defclass vertex-array (gl-object)
  ((vertex-count :initform 0 :initarg :vertex-count))
  (:default-initargs :id (gl:gen-vertex-array)))

(defun make-vertex-array (vertex-count)
  (make-instance 'vertex-array :vertex-count vertex-count))

(defmacro with-bound-vertex-array ((vertex-array) &body body)
  `(unwind-protect
        (progn
          (gl:bind-vertex-array (id-of ,vertex-array))
          ,@body)
     (gl:bind-vertex-array 0)))

(defmethod render ((this vertex-array))
  (with-bound-vertex-array (this)
    (with-slots (vertex-count) this
      (gl:draw-arrays :triangle-strip 0 vertex-count))))
