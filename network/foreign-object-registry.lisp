(in-package :cl-bodge.network)


(definline make-foreign-object-registry ()
  (make-hash-table))


(definline %pointer-address (foreign-object ptr)
  (cffi:pointer-address (if ptr ptr (ptr (handle-value-of foreign-object)))))


(defun register-foreign-object (registry object &optional ptr)
  (setf (gethash (%pointer-address object ptr) registry) object))


(defun find-foreign-object (registry pointer)
  (gethash (cffi:pointer-address pointer) registry))


(defun unregister-foreign-object (registry object &optional ptr)
  (remhash (%pointer-address object ptr) registry)
  object)
