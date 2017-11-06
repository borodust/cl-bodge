;;;
;;; SBCL-only code allowed w/o feature testing
;;;

(cl:defpackage :cl-bodge.distribution.build
  (:use :cl))
(cl:in-package :cl-bodge.distribution.build)

(declaim (special *assets-path*))

(pushnew :bodge-production-mode *features*)

(setf sb-ext:*muffled-warnings* 'style-warning)

(import 'bodge-asset-path :cl-user)
(defun bodge-asset-path ()
  *assets-path*)
