(cl:in-package :cl-bodge.math)


(definline vec3 (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-wrapped 'vec3 (v3:make (f x) (f y) (f z))))


(definline copy-vec3 (vec)
  (make-wrapped 'vec3 (v3:make (vref vec 0) (vref vec 1) (vref vec 2))))


(defmethod vector-length ((this vec3))
  (v3:length (value-of this)))


(defgeneric value->vec3 (val &key &allow-other-keys))


(defmethod value->vec3 ((vec vec4) &key)
  (make-wrapped 'vec3 (v3:make (vref vec 0) (vref vec 1) (vref vec 2))))


(defmethod value->vec3 ((vec vec3) &key)
  (copy-vec3 vec))


(defmethod value->vec3 ((vec vec2) &key (z 0f0))
  (make-wrapped 'vec3 (v3:make (vref vec 0) (vref vec 1) (f z))))


(definline sequence->vec3 (seq)
  (vec3 (elt seq 0)
        (elt seq 1)
        (elt seq 2)))

;;;
;;; VEC3
;;;
(defmethod addere ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:+ (value-of this) (value-of that))))


(defmethod addere ((this vec3) (scalar number))
  (make-wrapped 'vec3 (v3:+s (value-of this) (f scalar))))


(defmethod addere ((scalar number) (this vec3))
  (addere this scalar))


(defmethod subtract ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:- (value-of this) (value-of that))))


(defmethod subtract ((this vec3) (scalar number))
  (make-wrapped 'vec3 (v3:-s (value-of this) (f scalar))))


(defmethod subtract ((scalar number) (this vec3))
  (multiply (subtract this scalar) -1))


(defmethod lerp ((this vec3) (that vec3) (f number))
  (make-wrapped 'vec3 (v3:lerp (value-of this) (value-of that) (f f))))


(defmethod normalize ((this vec3))
  (make-wrapped 'vec3 (v3:normalize (value-of this))))


(defmethod multiply ((this vec3) (scalar number))
  (make-wrapped 'vec3 (v3:*s (value-of this) (f scalar))))


(defmethod multiply ((scalar number) (this vec3))
  (multiply this scalar))


(defmethod multiply ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:* (value-of this) (value-of that))))


(defmethod divide ((this vec3) (scalar number))
  (make-wrapped 'vec3 (v3:/s (value-of this) (f scalar))))


(defmethod divide ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:/s (value-of this) (value-of that))))


(defmethod cross-product ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:cross (value-of this) (value-of that))))


(defmethod dot-product ((this vec3) (that vec3))
  (make-wrapped 'vec3 (v3:dot (value-of this) (value-of that))))
