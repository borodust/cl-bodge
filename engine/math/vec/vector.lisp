(cl:in-package :cl-bodge.math)


(definline vec->array (vec)
  (value-of vec))


(defgeneric vector-length (vec))


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


(definline x (vec)
  (v:x (value-of vec)))


(definline (setf x) (value vec)
  (setf (v:x (value-of vec)) (f value)))


(definline y (vec)
  (v:y (value-of vec)))


(definline (setf y) (value vec)
  (setf (v:y (value-of vec)) (f value)))


(definline z (vec)
  (v:z (value-of vec)))


(definline (setf z) (value vec)
  (setf (v:z (value-of vec)) (f value)))


(definline w (vec)
  (v:w (value-of vec)))


(definline (setf w) (value vec)
  (setf (v:w (value-of vec)) (f value)))
