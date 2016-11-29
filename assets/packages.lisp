(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.assets
  (:nicknames :ge.as)
  (:use :cl :cl-bodge.utils :cl-bodge.graphics :cl-bodge.graphics.resources :cl-bodge.engine
        :cl-bodge.resources)
  (:export asset-system
           build-shading-program))
