(cl:defpackage :cl-bodge.distribution
  (:nicknames :ge.dist)
  (:use :cl :alexandria :asdf)
  (:export descriptor
           configure-system
           *lisp*
           *zip*
           register-distribution
           make-distribution))
