(bodge-util:define-package :cl-bodge.appkit
  (:nicknames :ge.app)
  (:use :cl :cl-bodge.engine :bodge-util :cl-bodge.resources)
  (:export start
           stop

           defapp
           app
           app-ui
           app-canvas

           configuration-flow
           sweeping-flow
           acting-flow
           inject-flow

           draw))
