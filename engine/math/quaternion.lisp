(cl:in-package :cl-bodge.math)


(defun quat (x y z s)
  (make-instance 'quat :value (q:q! x y z s)))


(defun identity-quat ()
  (make-instance 'quat :value (q:identity)))


(defun sequence->quat (sequence)
  (quat (elt sequence 0)
        (elt sequence 1)
        (elt sequence 2)
        (elt sequence 3)))


(defun euler-axis->quat (a vec3)
  (make-instance 'quat :value (q:from-axis-angle (value-of vec3) a)))


(defun euler-angles->quat (x y z)
  (make-instance 'quat :value (q:from-fixed-angles x y z)))


(defmethod normalize ((this quat))
  (make-instance 'quat :value (q:normalize (value-of this))))


(defmethod multiply ((this quat) (that quat))
  (make-instance 'quat :value (q:* (value-of this) (value-of that))))


(defmethod multiply ((this quat) (that number))
  (make-instance 'quat :value (q:*s (value-of this) (f that))))


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


(defmethod lerp ((this quat) (that quat) (f number))
  (make-instance 'quat :value (q:lerp (value-of this) (value-of that) (f f))))


(defun rotate (vec3 quat)
  (make-instance 'vec3 :value (q:rotate (value-of vec3 ) (value-of quat))))
