(in-package :cl-bodge.scene)


(defclass rendering-pass (system-scene-pass)
  ((host :initform (host) :reader host-of)
   (clear-color :initarg :clear-color)))


(defmethod initialize-instance ((this rendering-pass) &rest keys &key)
  (apply #'call-next-method this :system (graphics) keys))


(defun make-rendering-pass (&key (clear-color (vec4 1.0 1.0 1.0 1.0)))
  (make-instance 'rendering-pass
                 :clear-color clear-color))


(defmethod run-scene-pass ((this rendering-pass) root)
  (with-slots (clear-color) this
    (gl:clear-color (x clear-color) (y clear-color) (z clear-color) (w clear-color)))
  (gl:clear :color-buffer :depth-buffer)
  (call-next-method)
  (swap-buffers (host-of this)))
