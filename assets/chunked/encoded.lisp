(in-package :cl-bodge.assets)


(defmethod read-chunk ((type (eql :encoded)) parameters stream)
  (destructuring-bind (&key name size &allow-other-keys) parameters
    (decode-resource (find-resource-handler name)
                     (flex:make-flexi-stream stream
                                             :element-type '(unsigned-byte 8)
                                             :bound size))))
