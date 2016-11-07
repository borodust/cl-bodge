(in-package :cl-bodge.resources)


(defclass resource-system (enableable generic-system) ()
  (:default-initargs :depends-on '(graphics-system)))


(defmethod discard-system :before ((this resource-system))
  (clear-all-library-caches))
