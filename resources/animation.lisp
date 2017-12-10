(in-package :cl-bodge.resources)


;;;
;;; Animation resource
;;;
(defclass animation-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :animation))


(defmethod make-resource-handler ((type (eql :animation)) &key)
  (make-instance 'animation-resource-handler))
