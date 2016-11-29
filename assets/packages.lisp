(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.assets
  (:nicknames :ge.as)
  (:use :cl-bodge.utils :cl-bodge.graphics :cl-bodge.graphics.resources :cl-bodge.math
        :cl-bodge.concurrency :cl-bodge.memory :cl-bodge.engine :cl-bodge.resources
        :cl :cl-muth)
  (:export asset-system
           build-shading-program))
