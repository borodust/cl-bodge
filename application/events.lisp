(in-package :cl-bodge.application)


(defclass input-event (event) ())


(defevent keyboard-event (input-event) ())


(defevent mouse-event (input-event) ())


(defevent framebuffer-size-change-event (event)
  (width height))
