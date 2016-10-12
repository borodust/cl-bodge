(in-package :cl-bodge.concurrency)


(defgeneric execute (obj fn))


(defmacro -> (place &body body)
  `(execute ,place (lambda () ,@body)))
