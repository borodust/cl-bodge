(in-package :cl-bodge.asdf)



(ge.util:reexporting (:cl-bodge.animation
                      :cl-bodge.assets
                      :cl-bodge.audio
                      :cl-bodge.audio.resources
                      :cl-bodge.engine
                      :cl-bodge.event
                      :cl-bodge.graphics
                      :cl-bodge.graphics.resources
                      :cl-bodge.host
                      :cl-bodge.physics
                      :cl-bodge.resources
                      :cl-bodge.scene) :cl-bodge
  (defpackage :cl-bodge
    (:nicknames :ge)))
