
(ge.util:define-package :cl-bodge.host
  (:nicknames :ge.host)
  (:use :cl-bodge.engine :cl-bodge.utils :cl :bordeaux-threads)
  (:reexport-from :bodge-host
                  #:list-controllers
                  #:controller-name
                  #:controller-axes
                  #:controller-axis-value
                  #:controller-buttons
                  #:controller-button-pressed-p
                  #:controller-hats
                  #:controller-hat-state

                  #:list-gamepads
                  #:gamepad-name
                  #:gamepad-state
                  #:gamepad-state-button-pressed-p
                  #:gamepad-state-dpad
                  #:gamepad-state-left-stick
                  #:gamepad-state-right-stick
                  #:gamepad-state-left-trigger
                  #:gamepad-state-right-trigger)
  (:export #:host-system
           #:host
           #:for-host

           #:make-shared-rendering-context
           #:bind-rendering-context
           #:release-rendering-context
           #:swap-buffers
           #:swap-interval
           #:viewport-title
           #:viewport-size
           #:with-viewport-dimensions
           #:framebuffer-size
           #:with-framebuffer-dimensions
           #:hotkey-state
           #:fullscreen-viewport-p
           #:lock-cursor
           #:unlock-cursor
           #:viewport-scale

           #:cursor-position
           #:keyboard-button-state
           #:mouse-button-state

           #:state-from
           #:keyboard-event
           #:key-from
           #:character-input-event
           #:character-from
           #:mouse-event
           #:button-from
           #:cursor-event
           #:x-from
           #:y-from
           #:scroll-event
           #:x-offset-from
           #:y-offset-from
           #:viewport-size-change-event
           #:framebuffer-size-change-event
           #:width-from
           #:height-from
           #:viewport-hiding-event

           #:controller-connected-event
           #:controller-disconnected-event
           #:controller-from

           #:make-input-map
           #:enable-input-map
           #:disable-input-map
           #:bind-mouse
           #:bind-mouse-button
           #:bind-keyboard
           #:bind-keyboard-button
           #:bind-keyboard-hotkey
           #:bind-characters
           #:bind-cursor
           #:bind-scroll))
