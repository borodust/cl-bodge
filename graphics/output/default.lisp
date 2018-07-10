(cl:in-package :cl-bodge.graphics)

;;;
;;; DEFAULT FRAMEBUFFER
;;;
(defmethod %clear-rendering-output ((this t) color)
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color (x color) (y color) (z color) (w color))
  (gl:clear :color-buffer :stencil-buffer :depth-buffer))


(defmethod run-with-bound-output ((output t) function)
  (reset-viewport)
  (gl:bind-framebuffer :framebuffer 0)
  (funcall function))
