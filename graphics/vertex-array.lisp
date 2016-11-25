(in-package :cl-bodge.graphics)


(declaim (special *active-vertex-array*))


(defclass vertex-array (gl-object)
  ((vertex-count :initform 0 :initarg :vertex-count :reader vertex-count-of))
  (:default-initargs :id (gl:gen-vertex-array)))


(define-destructor vertex-array ((id id-of) (sys system-of))
  (-> (sys)
    (gl:delete-vertex-arrays (list id))))


(defun make-vertex-array (system vertex-count)
  (make-instance 'vertex-array :system system :vertex-count vertex-count))


(defmacro with-bound-vertex-array ((vertex-array) &body body)
  (once-only (vertex-array)
    `(unwind-protect
          (let ((*active-vertex-array* ,vertex-array))
            (gl:bind-vertex-array (id-of ,vertex-array))
            ,@body)
       (gl:bind-vertex-array (if-bound *active-vertex-array*
                                       (id-of *active-vertex-array*)
                                       0)))))


(defmethod attach-gpu-buffer ((buffer array-buffer) (vao vertex-array))
  (when (/= (vertex-count-of vao) (vertex-count-of buffer))
    (error "Vertex count of vertex array is different fron vertex count of buffer"))
  (with-bound-vertex-array (vao)
    (with-bound-buffer (buffer)
      (gl:vertex-attrib-pointer (vertex-attribute-index-of buffer)
                                (attribute-size-of buffer) :float nil 0 0))
    (gl:enable-vertex-attrib-array (vertex-attribute-index-of buffer))))
