(in-package :cl-user)

;;;
;;; SBCL-only code allowed w/o feature testing
;;;
(declaim (special *assets-path*))


(defun load-engine-assets ()
  (ge.as:mount-container "/_engine/" (ge.ng:merge-working-pathname *assets-path*) "/_engine/"))


(pushnew #'load-engine-assets ge.ng:*engine-startup-hooks*)
