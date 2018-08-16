(cl:in-package :cl-bodge.math)


(definline vec4 (&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0))
  (make-wrapped 'vec4 (v4:make (f x) (f y) (f z) (f w))))


(definline copy-vec4 (vec)
  (make-wrapped 'vec4 (v4:make (vref vec 0) (vref vec 1) (vref vec 2) (vref vec 3))))


(defmethod vector-length ((this vec4))
  (v4:length (value-of this)))


(defgeneric value->vec4 (val &key &allow-other-keys))


(defmethod value->vec4 ((vec vec3) &key (w 0.0))
  (make-wrapped 'vec4 (v4:make (vref vec 0) (vref vec 1) (vref vec 2) (f w))))


(defmethod value->vec4 ((vec vec2) &key (z 0f0) (w 0.0))
  (make-wrapped 'vec4 (v4:make (vref vec 0) (vref vec 1) (f z) (f w))))


(defmethod value->vec4 ((vec vec4) &key)
  (copy-vec4 vec))


(definline sequence->vec4 (seq)
  (vec4 (elt seq 0)
        (elt seq 1)
        (elt seq 2)
        (elt seq 3)))


;;;
;;; VEC4
;;;
(defmethod addere ((this vec4) (that vec4))
  (make-wrapped 'vec4 (v4:+ (value-of this) (value-of that))))


(defmethod addere ((this vec4) (scalar number))
  (make-wrapped 'vec4 (v4:+s (value-of this) (f scalar))))


(defmethod addere ((scalar number) (this vec4))
  (addere this scalar))


(defmethod subtract ((this vec4) (that vec4))
  (make-wrapped 'vec4 (v4:- (value-of this) (value-of that))))


(defmethod subtract ((this vec4) (scalar number))
  (make-wrapped 'vec4 (v4:-s (value-of this) (f scalar))))


(defmethod subtract ((scalar number) (this vec4))
  (multiply (subtract this scalar) -1))


(defmethod multiply ((this vec4) (scalar number))
  (make-wrapped 'vec4 (v4:*s (value-of this) (f scalar))))


(defmethod multiply ((scalar number) (this vec4))
  (multiply this scalar))


(defmethod divide ((this vec4) (scalar number))
  (make-wrapped 'vec4 (v4:/s (value-of this) (f scalar))))


(defmethod divide ((this vec4) (that vec4))
  (make-wrapped 'vec4 (v4:/ (value-of this) (value-of that))))


(defmethod lerp ((this vec4) (that vec4) (f number))
  (make-wrapped 'vec4 (v4:lerp (value-of this) (value-of that) (f f))))


(defmethod normalize ((this vec4))
  (make-wrapped 'vec4 (v4:normalize (value-of this))))


(defmethod dot-product ((this vec4) (that vec4))
  (make-wrapped 'vec4 (v4:dot (value-of this) (value-of that))))
