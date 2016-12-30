(in-package :cl-bodge.concurrency)


(defclass lockable ()
  ((lock :initform (bt:make-recursive-lock "instance-lock") :reader instance-lock-of)))


(defmacro with-instance-lock-held ((instance) &body body)
  `(bt:with-recursive-lock-held ((instance-lock-of ,instance))
     ,@body))
