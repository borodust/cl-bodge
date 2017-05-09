(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge
  (:nicknames :ge)
  (:use-reexport :cl-bodge.animation
                 :cl-bodge.assets
                 :cl-bodge.audio
                 :cl-bodge.engine
                 :cl-bodge.network
                 :cl-bodge.graphics
                 :cl-bodge.host
                 :cl-bodge.physics
                 :cl-bodge.resources
                 :cl-bodge.scene
                 :cl-bodge.poiu
                 :cl-bodge.text
                 :cl-bodge.canvas
                 :cl-bodge.interactions
                 :cl-bodge.library.shading))
