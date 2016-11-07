(in-package :cl-bodge.math)


(definline vec->array (vec)
  (value-of vec))


(definline make-vec3* (&optional (x #f0) (y #f0) (z #f0))
  (make-instance 'vec3 :value (v3:make x y z)))


(definline make-vec4* (&optional (x #f0) (y #f0) (z #f0) (w #f0))
  (make-instance 'vec4 :value (v4:make x y z w)))


(definline make-vec2* (&optional (x #f0) (y #f0))
  (make-instance 'vec2 :value (v2:make x y)))


(definline make-vec3 (vec)
  (make-instance 'vec3 :value (v3:make (vref vec 0) (vref vec 1) (vref vec 2))))


(definline sequence->vec3 (seq)
  (make-vec3* (elt seq 0)
              (elt seq 1)
              (elt seq 2)))


(defun vref (vec idx)
  (let ((vec (value-of vec)))
    (ecase idx
      (0 (v:x vec))
      (1 (v:y vec))
      (2 (v:z vec))
      (3 (v:w vec)))))


(defun (setf vref) (value vec idx)
  (let ((vec (value-of vec)))
    (ecase idx
      (0 (setf (v:x vec) value))
      (1 (setf (v:y vec) value))
      (2 (setf (v:z vec) value))
      (3 (setf (v:w vec) value)))))


(defmethod summarize ((this vec3) (that vec3))
  (make-instance 'vec3 :value (v3:+ (value-of this) (value-of that))))


(defmethod lerp ((this vec3) (that vec3) f)
  (make-instance 'vec3 :value (v3:lerp (value-of this) (value-of that) f)))
