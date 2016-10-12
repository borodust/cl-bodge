(in-package :cl-bodge.graphics)

;;
;;
(defclass buffer (gl-object)
  ((target :initarg :target :initform (error "Buffer target should be provided")
           :reader target-of))
  (:default-initargs :id (gl:gen-buffer)))


(defgeneric attach-gpu-buffer (buffer target))


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
   (vertex-count :initform 0 :reader vertex-count-of))
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


;;;
;;;
;;;
(defclass index-buffer (buffer)
  ((index-count :initform nil :reader index-count-of))
  (:default-initargs :target :element-array-buffer))


(defmethod initialize-instance :after ((this index-buffer) &key index-array)
  (with-slots (index-count) this
    (setf index-count (length index-array))
    (gl:with-gl-array (gl-array :uint :count index-count)
      (map-to-gl-array index-array gl-array)
      (with-bound-buffer (this)
        (gl:buffer-data :element-array-buffer :static-draw gl-array)))))


(declaim (ftype (function ((simple-array integer (*))) *) make-index-buffer)
         (inline make-index-buffer))
(defun make-index-buffer (index-array)
  (make-instance 'index-buffer :index-array index-array))
