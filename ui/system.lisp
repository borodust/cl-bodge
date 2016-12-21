(in-package :cl-bodge.interactions)


(defclass poiu-system (thread-bound-system) ()
  (:default-initargs :depends-on '(graphics-system host-system asset-system event-system)))
