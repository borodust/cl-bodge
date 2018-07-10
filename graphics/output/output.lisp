(cl:in-package :cl-bodge.graphics)


(declaim (special *supplementary-framebuffer*
                  *depth-stencil-renderbuffer*
                  *current-color-attachment-index*))


(defvar *default-clear-color* (vec4 1 1 1 1))
(defvar *active-framebuffer* 0)

(defgeneric run-with-bound-output (output function))
(defgeneric %clear-rendering-output (output color))

(defun clear-rendering-output (output &key color)
  (%clear-rendering-output output (or color *default-clear-color*)))


(definline index->color-attachment (idx)
  (+ (cffi:foreign-enum-value '%gl:enum :color-attachment0) idx))
