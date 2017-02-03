(in-package :cl-bodge.scene)



;;;
;;;
;;;
(defclass texture-node (scene-node)
  ((tex :initarg :texture)
   (unit :initarg :unit :initform 0)))


(defmethod scene-pass ((this texture-node) (pass rendering-pass) input)
  (with-slots (tex unit) this
    (with-bound-texture (tex unit)
      (call-next-method))))
