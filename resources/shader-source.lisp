(in-package :cl-bodge.resources)


(defenum shader-source-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader)


(defclass shader-source ()
  ((type :initarg :type :reader ge.gx.rsc:shader-type-of)
   (text :initarg :text :reader ge.gx.rsc:shader-text-of)))


(declaim (inline read-file-into-string-list))
(defun read-file-into-string-list (pathname)
  (split-sequence:split-sequence "#\Newline" (read-file-into-string pathname)))


(declaim (ftype (function (shader-source-type *) *) load-shader-source)
         (inline load-shader-source))
(defun load-shader-source (type pathspec)
  (make-instance 'shader-source :type type :text (read-file-into-string-list pathspec)))
