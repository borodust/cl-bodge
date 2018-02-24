(ge.util:define-package :cl-bodge.host
  (:nicknames :ge.host)
  (:use :cl-bodge.engine :cl-bodge.utils :cl :bordeaux-threads :cl-muth)
  (:export host-system
           host
           for-host

           bind-rendering-context
           release-rendering-context
           swap-buffers
           swap-interval
           viewport-title
           viewport-size
           framebuffer-size
           cursor-position
           mouse-button-state
           hotkey-state
           fullscreen-viewport-p
           lock-cursor
           unlock-cursor

           state-from
           keyboard-event
           key-from
           character-input-event
           character-from
           mouse-event
           button-from
           cursor-event
           x-from
           y-from
           scroll-event
           x-offset-from
           y-offset-from
           viewport-size-change-event
           framebuffer-size-change-event
           width-from
           height-from
           viewport-hiding-event

           make-input-map
           enable-input-map
           disable-input-map
           bind-mouse
           bind-mouse-button
           bind-keyboard
           bind-keyboard-button
           bind-keyboard-hotkey
           bind-characters
           bind-cursor))
