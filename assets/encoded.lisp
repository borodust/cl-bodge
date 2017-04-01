(in-package :cl-bodge.assets)


(defmethod read-chunk ((type (eql :encoded)) parameters stream)
  (destructuring-bind (&key name asset-class &allow-other-keys) parameters
    (decode-asset asset-class stream)))
