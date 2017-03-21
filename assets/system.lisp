(in-package :cl-bodge.assets)


(defclass asset-system (enableable generic-system)
  ((asset-registry :initform nil :reader asset-registry-of)))


(definline assets ()
  (engine-system 'asset-system))


(defmethod initialize-system :after ((this asset-system))
  (with-slots (asset-registry) this
    (setf asset-registry (make-instance 'asset-registry))))


(defun asset-flow (&rest asset-names)
  (let ((reg (asset-registry-of (assets))))
    (>> (~> (loop for name in asset-names
               collecting (get-asset reg name)))
        (instantly (assets)
          (values-list (mapcar #'first assets))))))
