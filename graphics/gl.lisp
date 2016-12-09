(in-package :cl-bodge.graphics)


(defgeneric render (renderable))


(defclass gl-object (foreign-object) ())



;; up to 2 dimensions
(defun map-to-gl-array (array gl-array)
  (destructuring-bind (vertex-count &optional (attrib-size 1 attrib-size-present-p))
      (array-dimensions array)
    (loop for i from 0 below vertex-count do
         (if attrib-size-present-p
             (loop for j from 0 below attrib-size do
                  (setf (gl:glaref gl-array (+ (* i attrib-size) j))
                        (aref array i j)))
             (setf (gl:glaref gl-array i) (aref array i))))))
