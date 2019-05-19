(cl:in-package :cl-bodge.host)


(defevent input-event () ())


(defevent keyboard-event (input-event)
  (key state))


(defevent character-input-event (input-event)
  (character))


(defevent mouse-event (input-event)
  (button state))


(defevent cursor-event (input-event)
  (x y))


(defevent viewport-size-change-event ()
  (width height))


(defevent framebuffer-size-change-event ()
  (width height))


(defevent scroll-event (input-event)
  (x-offset y-offset))


(defevent viewport-hiding-event () ())


(defevent controller-connected-event ()
  (controller))


(defevent controller-disconnected-event ()
  (controller))


(defevent gamepad-connected-event ()
  (gamepad))


(defevent gamepad-disconnected-event ()
  (gamepad))
