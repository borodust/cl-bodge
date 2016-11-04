(in-package :cl-bodge.math)


(defun make-quat* (x y z)
  (make-instance 'quat :value (q:from-fixed-angles x y z)))


(defun make-quat-from-euler-axis (a x y z)
  (make-instance 'quat :value (q:from-axis-angle (v3:make x y z) a)))


(defmethod normalize ((this quat))
  (make-instance 'quat :value (q:normalize (value-of this))))


(defmethod multiply ((this quat) (that quat))
  (make-instance 'quat :value (q:* (value-of this) (value-of that))))


(defmethod multiply ((this quat) (that number))
  (make-instance 'quat :value (q:*s (value-of this) #f that)))


(defmethod multiply ((that number) (this quat))
  (multiply this that))


(defmethod subtract ((this quat) (that quat))
  (make-instance 'quat :value (q:- (value-of this) (value-of that))))


(defmethod summarize ((this quat) (that quat))
  (make-instance 'quat :value (q:+ (value-of this) (value-of that))))


(defun quat->rotation-mat3 (q)
  (make-instance 'mat3 :value (q:to-mat3 (value-of q))))


(defun quat->rotation-mat4 (q)
  (make-instance 'mat4 :value (q:to-mat4 (value-of q))))


(defmethod lerp ((this quat) (that quat) f)
  (make-instance 'quat :value (q:lerp (value-of this) (value-of that) f)))
