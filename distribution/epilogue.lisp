(in-package :cl-user)

;;;
;;; SBCL-only code allowed w/o feature testing
;;;

(in-package :cl-bodge.distribution.build)

(ge.rsc:unmount-all)

(defun load-engine-assets ()
  (ge.as:mount-container "/_engine/"
                         (ge.ng:merge-working-pathname (bodge-asset-path))
                         "/_engine/"))

(pushnew #'load-engine-assets ge.ng:*engine-startup-hooks*)
