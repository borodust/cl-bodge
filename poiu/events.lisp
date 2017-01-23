(in-package :cl-bodge.poiu)


(defevent button-click-event ()
  (poiu-button))


(defun register-poiu-events (event-system)
  (register-event-classes event-system
                          'button-click-event))
