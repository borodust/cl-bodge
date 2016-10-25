(in-package :cl-bodge.resources)


(defenum shader-source-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader)


(defclass shader-source ()
  ((type :initarg :type :reader shader-type-of)
   (path :initarg :path :initform (error "Source path must be supplied") :reader shader-path-of)
   (text :initarg :text :reader shader-text-of)))


(defmethod shader-name-of ((this shader-source))
  (with-slots (path) this
    (file-namestring (fad:canonical-pathname path))))


(declaim (ftype (function (shader-source-type *) *) load-shader-source)
         (inline load-shader-source))
(defun load-shader-source (type pathspec)
  (make-instance 'shader-source :type type
                 :path (fad:canonical-pathname pathspec)
                 :text (read-file-into-string pathspec)))


(defmethod reload-shader-text (shader-source)
  (with-slots (text path) shader-source
    (setf text (read-file-into-string path)))
  shader-source)
