(in-package :cl-bodge.graphics)

;;
;;
(defclass buffer (gl-object)
  ((target :initarg :target :initform (error "Buffer target should be provided")
           :reader target-of))
  (:default-initargs :id (gl:gen-buffer)))


(defgeneric attach-buffer (buffer target))


(defmacro with-bound-buffer ((buffer) &body body)
  (once-only (buffer)
    `(unwind-protect
          (progn
            (gl:bind-buffer (target-of ,buffer) (id-of ,buffer))
            ,@body)
       (gl:bind-buffer (target-of ,buffer) 0))))

;;
;;
(defclass array-buffer (buffer)
  ((vertex-attribute-index :initarg :vertex-attribute-index
                           :initform (error "Vertex attribute index should be provided")
                           :reader vertex-attribute-index-of)
   (attribute-size :initform 0 :reader attribute-size-of)
   (vertex-count :initform 0))
  (:default-initargs :target :array-buffer))


(defun make-array-buffer (vertex-attribute-index vertex-attribute-data)
  (make-instance 'array-buffer
                 :vertex-attribute-data vertex-attribute-data
                 :vertex-attribute-index vertex-attribute-index))


(defmethod initialize-instance :after ((this array-buffer) &key vertex-attribute-data)
  (declare (type (or (simple-array * (*))
                     (simple-array * (* 1))
                     (simple-array * (* 2))
                     (simple-array * (* 3))
                     (simple-array * (* 4))) vertex-attribute-data))
  (destructuring-bind (data-vertex-count &optional (data-attrib-size 1))
      (array-dimensions vertex-attribute-data)
    (with-slots (attribute-size vertex-count) this
      (setf attribute-size data-attrib-size
            vertex-count data-vertex-count)
        (gl:with-gl-array (gl-array :float :count (* vertex-count attribute-size))
          (map-to-gl-array vertex-attribute-data gl-array)
          (with-bound-buffer (this)
            (gl:buffer-data :array-buffer :static-draw gl-array))))))


(defmethod attach-buffer ((buffer array-buffer) (vao vertex-array))
  (with-bound-vertex-array (vao)
    (with-bound-buffer (buffer)
      (gl:vertex-attrib-pointer (vertex-attribute-index-of buffer)
                                (attribute-size-of buffer) :float nil 0 0))
    (gl:enable-vertex-attrib-array (vertex-attribute-index-of buffer))))
      
