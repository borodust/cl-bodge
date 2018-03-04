(ge.util:define-package :cl-bodge.ui
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics
        :cl-bodge.canvas :cl-bodge.resources :claw)
  (:export make-ui-context
           with-ui-context
           with-ui-access

           defwindow
           make-window
           compose-window

           make-debug-console
           show-debug-console
           hide-debug-console
           add-simple-reporter

           next-keyboard-interaction
           next-mouse-interaction
           last-cursor-position
           next-character
           next-scroll

           make-host-input-source
           attach-host-input-source
           detach-host-input-source))
