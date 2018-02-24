(cl:in-package :cl-bodge.resources)


;;;
;;; Resource handler
;;;
(defclass mesh-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :mesh))


(defmethod make-resource-handler ((type (eql :mesh)) &key)
  (make-instance 'mesh-resource-handler))
