(cl:in-package :cl-bodge.utils)


(defun translate-name-to-foreign (symbol)
  (cffi:translate-name-to-foreign symbol *package*))


(defun translate-name-from-foreign (name)
  (cffi:translate-name-from-foreign name *package*))
