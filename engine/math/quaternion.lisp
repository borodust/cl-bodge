(cl:in-package :cl-bodge.math)


(definline quat (x y z s)
  (make-wrapped 'quat (q:q! (f x) (f y) (f z) (f s))))


(defun identity-quat ()
  (make-wrapped 'quat (q:identity)))


(defun sequence->quat (sequence)
  (quat (elt sequence 0)
        (elt sequence 1)
        (elt sequence 2)
        (elt sequence 3)))


(defun euler-axis->quat (a vec3)
  (make-wrapped 'quat (q:from-axis-angle (value-of vec3) a)))


(defun euler-angles->quat (vec3)
  (make-wrapped 'quat (q:from-fixed-angles (x vec3) (y vec3) (z vec3))))


(defmethod normalize ((this quat))
  (make-wrapped 'quat (q:normalize (value-of this))))


(defmethod multiply ((this quat) (that quat))
  (make-wrapped 'quat (q:* (value-of this) (value-of that))))


(defmethod multiply ((this quat) (that number))
  (make-wrapped 'quat (q:*s (value-of this) (f that))))


(defmethod multiply ((that number) (this quat))
  (multiply this that))


(defmethod subtract ((this quat) (that quat))
  (make-wrapped 'quat (q:- (value-of this) (value-of that))))


(defmethod addere ((this quat) (that quat))
  (make-wrapped 'quat (q:+ (value-of this) (value-of that))))


(defun quat->rotation-mat3 (q)
  (make-wrapped 'mat3 (q:to-mat3 (value-of q))))


(defun quat->rotation-mat4-homo (q)
  (make-wrapped 'mat3 (q:to-mat4 (value-of q))))


(defmethod lerp ((this quat) (that quat) (f number))
  (make-wrapped 'quat (q:lerp (value-of this) (value-of that) (f f))))


(defun rotate-vec3-with-quat (vec3 quat)
  (make-wrapped 'vec3 (q:rotate (value-of vec3) (value-of quat))))
