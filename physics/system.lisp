(in-package :cl-bodge.physics)


(defstruct (physics-context
             (:conc-name pc-)))
  
(defclass physics-system (thread-bound-system)
  ())


(defmethod initialize-system :after ((this physics-system))
  (ode:init))
  

(defmethod discard-system :after ((this physics-system))
  (ode:uninit))


(defmethod execute-looping-action ((this physics-system))
  (sleep 0.017))
