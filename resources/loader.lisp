(in-package :cl-bodge.resources)


;;;
;;;  Resource loader
;;;
(defgeneric list-resource-names (loader)
  (:method (loader) (declare (ignore loader)) nil))

(defgeneric load-resource (loader name))

(defgeneric release-resource (loader name)
  (:method (loader name) (declare (ignore loader name))))
