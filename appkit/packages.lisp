(ge.util:define-package :cl-bodge.appkit
  (:nicknames :ge.app)
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-bodge.resources)
  (:export start
           stop

           defapp
           when-app
           app
           app-ui
           app-canvas

           handle-drawing
           render-app-canvas

           configuration-flow
           sweeping-flow
           acting-flow
           inject-flow

           draw))
