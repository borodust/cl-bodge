(in-package :cl-bodge.concurrency)


(defclass lockable ()
  ((lock :initform (bt:make-recursive-lock "instance-lock") :reader instance-lock-of))
  (:documentation "Mixin for quick instance locking facility"))


(defmacro with-instance-lock-held ((instance) &body body)
  "Hold instance lock for the duration of the `body`."
  `(bt:with-recursive-lock-held ((instance-lock-of ,instance))
     ,@body))
