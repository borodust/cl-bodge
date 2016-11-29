(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl-bodge.utils :cl-bodge.graphics :cl-bodge.graphics.resources :cl-bodge.math
        :cl-bodge.concurrency :cl-bodge.memory :cl-bodge.engine :cl-bodge.audio.resources
        :cl :cl-muth :bodge-sndfile)
  (:export resource-system
           load-shader-source
           load-png-image
           load-ogg-vorbis-audio
           build-shading-program

           load-resource

           simple-model-chunks-of

           mesh-chunks-of
           mesh-chunk-id
           mesh-chunk-transform
           mesh-chunk-face
           mesh-chunk-arrays
           mesh-chunk-indexes
           mesh-chunk-bones
           mesh-bone-index
           mesh-bone-offset
           mesh-bone-bone

           skeleton-chunks-of
           skeleton-bone-id
           skeleton-bone-children
           skeleton-bone-transform

           animation-chunks-of
           animation-chunk-id
           animation-chunk-children
           keyframe-sequence-bone
           keyframe-sequence-children))
