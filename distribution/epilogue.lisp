(in-package :cl-user)

;;;
;;; SBCL-only code allowed w/o feature testing
;;;


(defun load-engine-assets ()
  (ge.rsc:register-resource-loader (ge.as:make-resource-loader *engine-assets-path*)))


(pushnew #'load-engine-assets *init-hooks*)
