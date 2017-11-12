(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.distribution
  (:nicknames :ge.dist)
  (:use :cl :alexandria :asdf)
  (:export descriptor
           configure-system
           *sbcl*
           *zip*
           register-distribution
           make-distribution))
