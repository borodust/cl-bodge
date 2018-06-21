(cl:in-package :cl-bodge.graphics)


(defgeneric enable-rendering-output (output))


(defmethod enable-rendering-output ((output t))
  (gl:bind-framebuffer :framebuffer 0))
