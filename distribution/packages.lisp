(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.distribution
  (:nicknames :ge.dist)
  (:use :cl :alexandria :asdf)
  (:export distribution
           make-distribution))
