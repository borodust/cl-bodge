(in-package :cl-bodge.library.shading)


(defun mount-text-file (resource-path file)
  (mount-filesystem resource-path file)
  (register-resource resource-path (make-text-resource-handler)))
