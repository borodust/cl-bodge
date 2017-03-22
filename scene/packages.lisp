(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.scene
  (:nicknames :ge.sg)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics :cl-bodge.physics
        :cl-bodge.host :cl-bodge.animation :cl-bodge.audio :cl-bodge.assets)
  (:export node
           find-node
           parent-of

           make-scene
           root-of
           scene-node
           scene-pass
           make-pass-chain
           scene-processing-flow
           node-enabled-p
           initialize-node
           discard-node
           discard-tree
           enableable-node
           enable-node
           disable-node

           *scene*
           *projection-matrix*
           *view-matrix*
           *model-matrix*
           model-view-projection-matrix

           rendering-pass
           make-rendering-pass
           shading-pipeline-node
           texture-node

           mesh-node
           make-node-mesh

           light-node
           *lights*
           apply-light-source-shading-parameter

           projection-node
           update-projection

           camera-node
           transform-of

           transform-node

           shading-program-node
           shading-parameter
           shading-parameters-node

           model
           model-graph-assembly-flow

           simulation-pass
           make-simulation-pass

           animation-node
           start-node-animation
           reset-node-animation

           *skeleton*
           animated-skeleton-node
           bone-transform
           bone-node
           root-bone-of

           banner-node

           scenegraph))
