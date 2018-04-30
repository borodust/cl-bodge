(cl:defpackage :cl-bodge.distribution.build
  (:use :cl))
(cl:in-package :cl-bodge.distribution.build)

(declaim (special cl-user::*bodge-asset-container-path*))

(pushnew :bodge-production-mode *features*)

#+sbcl
(setf sb-ext:*muffled-warnings* 'style-warning)

(defun bodge-asset-path ()
  cl-user::*bodge-asset-container-path*)
