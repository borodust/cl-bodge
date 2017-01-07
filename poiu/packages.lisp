(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.poiu
  (:nicknames :ge.poiu)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :plus-c
        :cl-bodge.canvas :autowrap :cl-bodge.text :cl-bodge.assets :cl-bodge.event)
  (:export make-poiu-context
           clear-poiu-context

           make-poiu
           compose-poiu
           name-of
           window
           make-window
           make-menu-bar
           make-static-row-layout
           make-dynamic-row-layout
           make-label-button
           make-text-label

           with-poiu-input
           register-cursor-position
           register-mouse-input

           button-click-event
           poiu-button-from
           register-poiu-events))
