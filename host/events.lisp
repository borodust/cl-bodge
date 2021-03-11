(cl:in-package :cl-bodge.host)


(defevent input-event () ())


(defevent keyboard-event (input-event)
  (key state modifiers))


(defevent character-input-event (input-event)
  (character modifiers))


(defevent mouse-event (input-event)
  (button state modifiers))


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


(defevent gamepad-event (input-event)
  (gamepad))


(defevent gamepad-button-event (gamepad-event)
  (button state))


(defevent gamepad-dpad-event (gamepad-event)
  (state))


(defevent gamepad-stick-event (gamepad-event)
  (x y))


(defevent gamepad-left-stick-event (gamepad-stick-event) ())


(defevent gamepad-right-stick-event (gamepad-stick-event) ())


(defevent gamepad-trigger-event (gamepad-event)
  (value))


(defevent gamepad-left-trigger-event (gamepad-trigger-event) ())


(defevent gamepad-right-trigger-event (gamepad-trigger-event) ())
