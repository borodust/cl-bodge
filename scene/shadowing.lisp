(in-package :cl-bodge.scene)


(defclass shadow-pass (system-scene-pass) ())


(defmethod initialize-instance ((this shadow-pass) &rest keys &key)
  (apply #'call-next-method this :system (graphics) keys))


(defmethod run-scene-pass ((this shadow-pass) root)
  (gl:clear :depth-buffer)
  (call-next-method))
