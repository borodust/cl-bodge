(ge.util:define-package :cl-bodge.ui
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics
        :cl-bodge.canvas :cl-bodge.resources :claw)
  (:export make-ui-context
           with-ui-access
           ui-font
           add-window
           remove-window

           compose-ui

           name-of
           hiddenp
           find-element

           make-container-layout

           defwindow
           make-window
           show-window
           hide-window

           make-menu-bar

           make-static-row-layout
           make-dynamic-row-layout

           show-widget
           hide-widget

           make-label-button
           make-text-label
           make-text-edit
           text-of
           make-spacing
           make-list-select
           add-item
           item-name-of

           make-debug-console
           show-debug-console
           hide-debug-console
           add-simple-reporter

           with-ui-input
           register-cursor-position
           register-mouse-input
           register-character-input
           register-keyboard-input))
