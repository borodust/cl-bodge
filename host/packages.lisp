
(ge.util:define-package :cl-bodge.host
  (:nicknames :ge.host)
  (:use :cl-bodge.engine :cl-bodge.utils :cl :bordeaux-threads)
  (:reexport-from :bodge-host
                  #:list-controllers
                  #:controller-name
                  #:controller-guid
                  #:controller-axes
                  #:controller-axis-value
                  #:controller-buttons
                  #:controller-button-pressed-p
                  #:controller-hats
                  #:controller-hat-state

                  #:list-gamepads
                  #:gamepad-name
                  #:gamepad-guid
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
           #:refresh-rate

           #:cursor-position
           #:keyboard-button-state
           #:mouse-button-state

           #:value-from
           #:state-from
           #:width-from
           #:height-from
           #:x-from
           #:y-from
           #:x-offset-from
           #:y-offset-from
           #:key-from
           #:character-from
           #:button-from
           #:controller-from
           #:gamepad-from

           #:keyboard-event
           #:character-input-event
           #:mouse-event
           #:cursor-event
           #:scroll-event
           #:viewport-size-change-event
           #:framebuffer-size-change-event
           #:viewport-hiding-event

           #:controller-connected-event
           #:controller-disconnected-event
           #:gamepad-connected-event
           #:gamepad-disconnected-event
           #:gamepad-button-event
           #:gamepad-dpad-event
           #:gamepad-left-stick-event
           #:gamepad-right-stick-event
           #:gamepad-left-trigger-event
           #:gamepad-right-trigger-event

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
