(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.assets)
  (:export load-resource
           find-chunk

           simple-model-chunks-of

           mesh-chunk-id
           mesh-chunk-transform
           mesh-chunk-face
           mesh-chunk-arrays
           mesh-chunk-indexes
           mesh-chunk-bones
           mesh-bone-index
           mesh-bone-offset
           mesh-bone-bone

           skeleton-bone-id
           skeleton-bone-children
           skeleton-bone-transform

           animation-chunk-id
           animation-chunk-children
           keyframe-sequence-bone
           keyframe-sequence-children

           image-chunk-name
           image-chunk-data
           image-chunk-width
           image-chunk-height
           image-chunk-data

           font-atlas-chunk-image-name
           font-atlas-chunk-ascender
           font-atlas-chunk-descender
           font-atlas-chunk-line-gap
           font-atlas-chunk-children

           glyph-metrics-character
           glyph-metrics-origin
           glyph-metrics-bounding-box
           glyph-metrics-advance-width
           glyph-metrics-kernings

           chunk->animation
           chunk->skeleton
           chunk->mesh
           chunk->image
           chunk->font))
