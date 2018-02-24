(cl:in-package :cl-bodge.resources)


(defclass skeleton-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :skeleton))


(defmethod make-resource-handler ((type (eql :skeleton)) &key)
  (make-instance 'skeleton-resource-handler))
