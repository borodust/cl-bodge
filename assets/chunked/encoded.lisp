(in-package :cl-bodge.assets)


(defmethod read-chunk ((type (eql :encoded)) parameters stream)
  (destructuring-bind (&key asset-class &allow-other-keys) parameters
    (decode-resource (apply #'make-resource-handler asset-class parameters) stream)))
