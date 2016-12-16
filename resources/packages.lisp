(in-package :cl-bodge.asdf)



(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl-bodge.utils :cl-bodge.graphics.resources :cl-bodge.audio.resources :cl-bodge.engine
        :cl :bodge-sndfile)
  (:export make-shader-source
           load-shader-source
           shader-path-of
           load-png-image
           load-ogg-vorbis-audio

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
           keyframe-sequence-children

           image-chunks-of
           image-chunk-name
           image-chunk-image

           font-atlas-chunks-of
           font-atlas-chunk-image-name
           font-atlas-chunk-ascender
           font-atlas-chunk-descender
           font-atlas-chunk-line-gap
           font-atlas-chunk-children

           glyph-metrics-character
           glyph-metrics-origin
           glyph-metrics-bounding-box
           glyph-metrics-advance-width
           glyph-metrics-kernings))
