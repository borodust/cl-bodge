(ge.util:define-package :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
  (:export engine-resource-name

           encode-resource
           decode-resource

           register-resource
           load-resource
           resource-flow
           list-registered-resource-names
           find-resource-handler

           path-node
           open-resource-stream
           mount-resource-provider
           mount-filesystem
           unmount-all

           defresource
           make-resource-handler
           make-text-resource-handler

           mount-container

           ;; audio
           load-ogg-vorbis-audio

           ;; images
           load-png-image

           ;; SDF font
           define-sdf-font
           sdf-font-atlas-resource-name
           sdf-font-metrics-resource-name
           font-atlas-chunk-ascender
           font-atlas-chunk-descender
           font-atlas-chunk-line-gap
           glyph-metrics-character
           glyph-metrics-origin
           glyph-metrics-bounding-box
           glyph-metrics-advance-width
           glyph-metrics-kernings

           ;; chunk-structure
           id-of
           children-of

           ;; ttf font
           font-container-data

           scene-meshes))
