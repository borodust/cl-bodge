(cl:in-package :cl-bodge.math)


(definline vec2 (&optional (x 0.0) (y 0.0))
  (make-wrapped 'vec2 (v2:make (f x) (f y))))


(definline copy-vec2 (vec)
  (make-wrapped 'vec2 (v2:make (vref vec 0) (vref vec 1))))


(defmethod vector-length ((this vec2))
  (v2:length (value-of this)))


(definline sequence->vec2 (seq)
  (vec2 (elt seq 0)
        (elt seq 1)))


(defgeneric value->vec2 (val &key &allow-other-keys))


(defmethod value->vec2 ((vec vec2) &key)
  (copy-vec2 vec))


(defmethod value->vec2 ((vec vec3) &key)
  (make-wrapped 'vec2 (v2:make (vref vec 0) (vref vec 1))))


(defmethod value->vec2 ((vec vec4) &key)
  (make-wrapped 'vec2 (v2:make (vref vec 0) (vref vec 1))))


(defmethod addere ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:+ (value-of this) (value-of that))))


(defmethod addere ((this vec2) (scalar number))
  (make-wrapped 'vec2 (v2:+s (value-of this) (f scalar))))


(defmethod addere ((scalar number) (this vec2))
  (addere this scalar))


(defmethod subtract ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:- (value-of this) (value-of that))))


(defmethod subtract ((this vec2) (scalar number))
  (make-wrapped 'vec2 (v2:-s (value-of this) (f scalar))))


(defmethod subtract ((scalar number) (this vec2))
  (multiply (subtract this scalar) -1))


(defmethod lerp ((this vec2) (that vec2) (f number))
  (make-wrapped 'vec2 (v2:lerp (value-of this) (value-of that) (f f))))


(defmethod normalize ((this vec2))
  (make-wrapped 'vec2 (v2:normalize (value-of this))))


(defmethod multiply ((this vec2) (scalar number))
  (make-wrapped 'vec2 (v2:*s (value-of this) (f scalar))))


(defmethod multiply ((scalar number) (this vec2))
  (multiply this scalar))


(defmethod multiply ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:* (value-of this) (value-of that))))


(defmethod divide ((this vec2) (scalar number))
  (make-wrapped 'vec2 (v2:/s (value-of this) (f scalar))))


(defmethod divide ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:/ (value-of this) (value-of that))))


(defmethod cross-product ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:cross (value-of this) (value-of that))))


(defmethod dot-product ((this vec2) (that vec2))
  (make-wrapped 'vec2 (v2:dot (value-of this) (value-of that))))
