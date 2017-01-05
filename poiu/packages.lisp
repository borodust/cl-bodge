(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.poiu
  (:nicknames :ge.poiu)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :plus-c
        :cl-bodge.canvas :autowrap :cl-bodge.text :cl-bodge.assets)
  (:export make-poiu-context
           with-poiu
           with-poiu-input
           in-window
           render-poiu
           clear-poiu

           register-cursor-position
           register-mouse-input))
