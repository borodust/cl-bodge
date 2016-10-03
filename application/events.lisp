(in-package :cl-bodge.application)


(defclass input-event (event) ())


(defclass keyboard-event (input-event) ())


(defclass mouse-event (input-event) ())
