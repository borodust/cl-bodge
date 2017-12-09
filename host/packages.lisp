(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.host
  (:nicknames :ge.host)
  (:use :cl-bodge.engine :cl-bodge.utils :cl :bordeaux-threads :cl-muth)
  (:export host-system
           host

           bind-rendering-context
           release-rendering-context
           swap-buffers
           swap-interval
           viewport-title
           viewport-size
           framebuffer-size
           cursor-position
           mouse-button-state
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

           make-keymap
           enable-keymap
           disable-keymap
           bind-button
           bind-key
           bind-cursor))
