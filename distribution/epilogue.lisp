(in-package :cl-user)

;;;
;;; SBCL-only code allowed w/o feature testing
;;;
(declaim (special *engine-assets-path*))


(defun load-engine-assets ()
  (ge.as:mount-container "/" (ge.ng:merge-working-pathname *engine-assets-path*)))


(pushnew #'load-engine-assets ge.ng:*engine-startup-hooks*)
