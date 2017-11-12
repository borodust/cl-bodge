(in-package :cl-bodge.distribution)


(defvar *distribution-registry* nil)


(defun %register-distribution (name distrib)
  (setf (assoc-value *distribution-registry* name :test #'equalp) distrib))


(defun distribution-by-name (name)
  (assoc-value *distribution-registry* name))
