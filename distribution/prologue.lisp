(in-package :cl-user)

;;;
;;; SBCL-only code allowed w/o feature testing
;;;

(declaim (special *engine-assets-path*))


(pushnew :bodge-production-mode *features*)


(setf *muffled-warnings* 'style-warning)
