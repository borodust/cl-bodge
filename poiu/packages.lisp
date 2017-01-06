(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.poiu
  (:nicknames :ge.poiu)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :plus-c
        :cl-bodge.canvas :autowrap :cl-bodge.text :cl-bodge.assets)
  (:export make-poiu-context
           clear-poiu-context

           make-poiu
           compose-poiu
           window
           make-window
           make-menu-bar
           make-static-row-layout
           make-label-button

           with-poiu-input
           register-cursor-position
           register-mouse-input))
