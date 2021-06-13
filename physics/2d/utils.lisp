(cl:in-package :cl-bodge.physics.chipmunk)


(defvar *zero-vec2* (vec2 0 0))


(definline cp-float (val)
  (float val 0f0))


(definline init-cp-vect (vect bodge-vec)
  (c-val ((vect %chipmunk:vect))
    (setf (vect :x) (cp-float (x bodge-vec))
          (vect :y) (cp-float (y bodge-vec))))
  vect)


(definline set-cp-vect (vect x y)
  (c-val ((vect %chipmunk:vect))
    (setf (vect :x) (cp-float x)
          (vect :y) (cp-float y)))
  vect)


(definline init-bodge-vec (bodge-vec cp-vect)
  (c-val ((cp-vect %chipmunk:vect))
    (setf (x bodge-vec) (cp-vect :x)
          (y bodge-vec) (cp-vect :y)))
  bodge-vec)


(defmacro with-cp-vect ((vect &optional bodge-vec) &body body)
  `(c-with ((,vect %chipmunk:vect))
     ,@(when bodge-vec
         `((init-cp-vect ,vect ,bodge-vec)))
     ,@body))


(defmacro with-cp-vects ((&rest defs) &body body)
  (labels ((%expand (defs body)
             (if defs
                 `((with-cp-vect (,@(ensure-list (first defs)))
                     ,@(%expand (rest defs) body)))
                 body)))
    (first (%expand defs body))))
