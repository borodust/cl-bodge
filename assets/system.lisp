(in-package :cl-bodge.assets)


(defclass asset-system (enableable generic-system) ()
  (:default-initargs :depends-on '(graphics-system)))


(defmethod discard-system :before ((this asset-system))
  (clear-all-caches))
