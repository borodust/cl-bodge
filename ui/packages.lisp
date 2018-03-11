(ge.util:define-package :cl-bodge.ui
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics
        :cl-bodge.canvas :cl-bodge.resources :claw)
  (:import-from :cl-bodge.host
                :button-from)
  (:export make-ui
           with-ui
           with-ui-access
           compose-ui
           defwindow
           on-window-close
           add-window
           remove-window

           vertical-layout
           horizontal-layout
           button
           label
           combo-box
           spacing
           color-picker
           float-property
           radio
           activated
           radio-group
           check-box
           checked
           custom-widget
           render-custom-widget
           custom-widget-hovered-p
           custom-widget-clicked-p
           custom-widget-pressed-p

           debug-console
           add-simple-reporter

           next-keyboard-interaction
           next-mouse-interaction
           last-cursor-position
           next-character
           next-scroll

           make-host-input-source
           attach-host-input-source
           detach-host-input-source

           source-from
           button-from))
