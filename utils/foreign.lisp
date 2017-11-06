(in-package :cl-bodge.utils)


(defun foreign-function-pointer (function-name)
  (when-let* ((fn (bodge-autowrap:find-function function-name)))
    (let ((name (bodge-autowrap:foreign-symbol-c-symbol fn)))
      (cffi-sys:%foreign-symbol-pointer name :default))))


(defmacro inhibiting-string-conversion ((object &rest path))
  `(,object ,@path bodge-plus-c:* bodge-plus-c:&))


(defun translate-name-to-foreign (symbol)
  (cffi:translate-name-to-foreign symbol *package*))


(defun translate-name-from-foreign (name)
  (cffi:translate-name-from-foreign name *package*))
