(cl:in-package :cl-bodge.math)


(definline mat->array (mat)
  (value-of mat))


(defgeneric mref (mat row column))


(defgeneric (setf mref) (value mat row column))
