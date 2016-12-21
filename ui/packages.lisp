(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.interactions
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.scene :cl-bodge.graphics :plus-c
        :cl-bodge.canvas :autowrap :cl-bodge.text :cl-bodge.assets :cl-bodge.host
        :cl-bodge.event)
  (:export make-poiu-context
           with-poiu
           in-window))
