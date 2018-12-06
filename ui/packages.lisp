(bodge-util:define-package :cl-bodge.ui
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :bodge-util :cl-bodge.graphics
        :cl-bodge.canvas :cl-bodge.resources :claw)
  (:reexport-from :bodge-ui
                  #:defpanel
                  #:on-window-close

                  #:horizontal-layout
                  #:vertical-layout

                  #:label
                  #:radio-group
                  #:radio
                  #:check-box
                  #:button

                  #:custom-widget
                  #:custom-widget-hovered-p
                  #:custom-widget-pressed-p
                  #:render-custom-widget)
  (:export #:make-ui
           #:compose-ui
           #:with-ui-access
           #:add-window
           #:remove-window
           #:remove-all-windows

           #:update-ui-size
           #:update-ui-pixel-ratio

           #:make-host-input-source
           #:attach-host-input-source
           #:detach-host-input-source))
