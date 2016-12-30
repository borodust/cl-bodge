(in-package :cl-bodge.assets)


(defclass asset-system (enableable generic-system)
  ((asset-registry :initform nil :reader asset-registry-of)))


(definline assets ()
  (engine-system 'asset-system))


(defmethod initialize-system :after ((this asset-system))
  (with-slots (asset-registry) this
    (setf asset-registry (make-instance 'asset-registry))))
