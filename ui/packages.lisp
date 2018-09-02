(bodge-util:define-package :cl-bodge.ui
  (:nicknames :ge.ui)
  (:use :cl :cl-bodge.engine :bodge-util :cl-bodge.graphics
        :cl-bodge.canvas :cl-bodge.resources :claw)
  (:export make-ui-canvas-renderer
           update-renderer-canvas-size
           update-renderer-canvas-pixel-ratio

           make-host-input-source
           attach-host-input-source
           detach-host-input-source))
