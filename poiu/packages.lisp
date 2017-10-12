(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.poiu
  (:nicknames :ge.poiu)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.graphics :bodge-plus-c
        :cl-bodge.canvas :bodge-autowrap :cl-bodge.text :cl-bodge.resources)
  (:export make-poiu-context
           clear-poiu-context

           make-poiu
           compose-poiu
           when-composing
           name-of
           find-element

           adopt-layout-by
           make-container-layout
           make-window
           defwindow
           show-window
           hide-window
           make-menu-bar
           make-static-row-layout
           make-dynamic-row-layout
           hiddenp
           show-widget
           hide-widget
           make-label-button
           make-text-label
           make-text-edit
           text-of
           make-spacing
           make-list-select
           add-item
           clear
           item-name-of
           make-health-monitor
           show-health-monitor
           hide-health-monitor
           add-simple-reporter

           with-poiu-input
           register-cursor-position
           register-mouse-input
           register-character-input
           register-keyboard-input

           button-click-event
           poiu-button-from
           item-selected
           source-from
           item-from))
