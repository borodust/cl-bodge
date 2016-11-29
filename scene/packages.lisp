(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.scene
  (:nicknames :ge.sg)
  (:use :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics :cl-bodge.physics
        :cl-bodge.math :cl-bodge.concurrency :cl-bodge.host :cl-bodge.memory
        :cl-bodge.animation :cl-bodge.resources :cl-bodge.audio :cl-bodge.assets
        :cl :cl-muth)
  (:export node
           find-node
           node-attaching
           node-detached
           parent-of

           make-scene
           root-of
           simulation-pass
           rendering-pass
           animate
           node-enabled-p
           initialize-node
           discard-node
           initialize-tree
           discard-tree


           *scene*
           *projection-matrix*
           *transform-matrix*

           body-transform-node
           shading-pipeline-node
           texture-node

           mesh-node
           make-node-mesh

           projection-node
           update-projection

           transform-node
           rotate-node
           translate-node

           camera-node
           translate-camera
           rotate-camera

           shading-program-node
           shading-parameter
           shading-parameters-node
           directional-light-node

           model
           make-model-graph

           animation-node
           start-node-animation
           reset-node-animation

           *skeleton*
           animated-skeleton-node
           bone-transform
           bone-node
           root-bone-of

           scenegraph

           chunk->animation
           chunk->skeleton
           chunk->mesh))
