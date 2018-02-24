(cl:in-package :cl-bodge.library.shading)


(defenum shader-source-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader)


(defclass shader-source ()
  ((type :initarg :type :reader shader-type-of)
   (name :initarg :name :reader shader-name-of)
   (text :initarg :text :reader shader-text-of)))


(defmethod reload-shader-text ((this shader-source)))


(defun make-shader-source (name type text)
  (make-instance 'shader-source :name name :type type :text text))


(defclass shader-source-file (shader-source)
  ((path :initarg :path :initform (error "Source path must be supplied")
         :reader shader-path-of)))


(declaim (ftype (function (shader-source-type *) *) load-shader-source)
         (inline load-shader-source))
(defun load-shader-source (type pathspec)
  (make-instance 'shader-source-file
                 :type type
                 :name (file-namestring (fad:canonical-pathname pathspec))
                 :path (fad:canonical-pathname pathspec)
                 :text (read-file-into-string pathspec)))


(defmethod reload-shader-text ((this shader-source-file))
  (with-slots (text path) this
    (setf text (read-file-into-string path)))
  this)
