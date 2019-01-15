(cl:in-package :cl-bodge.graphics)


(defun make-shader-structure (amalgam-class &rest args &key &allow-other-keys)
  (apply #'make-instance amalgam-class args))


(defmethod inject-shader-input ((this amalgam) &key name)
  (let ((fields (amalgam-descriptor-fields
                 (find-amalgam-descriptor (class-name-of this)))))
    (loop for field in fields
          for field-name = (amalgam-field-name field)
          for foreign-name = (amalgam-field-foreign-name field)
          for value = (slot-value this field-name)
          do (inject-shader-input value :name (format nil "~A.~A" name foreign-name)))))
