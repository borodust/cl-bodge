(cl:in-package :cl-bodge.ui)


(defevent ui-event ()
    (source))


(defevent button-clicked-event (ui-event)
    (button))


(defevent button-pressed-event (ui-event)
    (button))


(defevent button-released-event (ui-event)
    (button))
