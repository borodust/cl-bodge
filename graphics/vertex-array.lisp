(cl:in-package :cl-bodge.graphics)


(declaim (special *active-vertex-array*))


(defhandle vertex-array-handle
    :initform (gl:gen-vertex-array)
    :closeform (gl:delete-vertex-arrays (list *handle-value*)))


(defclass vertex-array (gl-object)
  ((vertex-count :initform 0 :initarg :vertex-count :reader vertex-count-of))
  (:default-initargs :handle (make-vertex-array-handle)))


(define-system-function make-vertex-array graphics-system (vertex-count &key (system *system*))
  (make-instance 'vertex-array :system system :vertex-count vertex-count))


(defmacro with-bound-vertex-array ((vertex-array) &body body)
  (once-only (vertex-array)
    `(unwind-protect
          (let ((*active-vertex-array* ,vertex-array))
            (gl:bind-vertex-array (handle-value-of ,vertex-array))
            ,@body)
       (gl:bind-vertex-array (if-bound *active-vertex-array*
                                       (handle-value-of *active-vertex-array*)
                                       0)))))


(defmethod attach-array-buffer ((buffer array-buffer) (vao vertex-array) index)
  (when (/= (vertex-count-of vao) (vertex-count-of buffer))
    (error "Vertex count of vertex array is different fron vertex count of buffer"))
  (with-bound-vertex-array (vao)
    (with-bound-buffer (buffer)
      (gl:vertex-attrib-pointer index (attribute-size-of buffer) :float nil 0 0))
    (gl:enable-vertex-attrib-array index)))
