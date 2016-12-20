(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.canvas
  (:nicknames :ge.vg)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics)
  (:export make-canvas
           with-canvas
           path
           draw-rect))
