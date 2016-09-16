(in-package :cl-bodge.package)


(defpackage :cl-bodge.bootstrap
  (:use :cl :trivial-main-thread)
  (:export))


(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))


