(in-package :cl-bodge.poiu)


(defevent button-click-event ()
  (poiu-button))


(defevent item-selected ()
  (source item))


(defun register-poiu-events (event-system)
  (register-event-classes event-system
                          'button-click-event
                          'item-selected))
