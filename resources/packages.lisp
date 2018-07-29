(ge.util:define-package :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :static-vectors)
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
           resource-type
           make-resource-handler
           make-text-resource-handler

           mount-container

           ;; audio
           load-ogg-vorbis-audio

           ;; images
           load-png-image

           ;; chunked
           write-chunk

           ;; ttf font
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

           ;; scene
           make-empty-scene-resource
           scene-resource-mesh
           scene-resource-material
           ;; mesh
           make-mesh-resource
           mesh-resource-position-array
           mesh-resource-index-array
           mesh-resource-normal-array
           mesh-resource-primitive
           ;; texture
           make-texture-resource
           texture-resource-name
           texture-resource-channel
           texture-resource-coord-id
           texture-resource-mapping-id
           texture-resource-mapping-name
           texture-resource-mapping-mode-u
           texture-resource-mapping-mode-v
           texture-resource-mapping-filter-mag
           texture-resource-mapping-filter-min
           texture-resource-scale
           texture-resource-strength
           ;; material
           make-material-resource
           material-resource-texture
           material-resource-name
           material-resource-shininess
           material-resource-diffuse-color
           material-resource-emissive-color
           material-resource-base-color-factor
           material-resource-metallic-factor
           material-resource-roughness-factor
           material-resource-glossiness-factor
           material-resource-alpha-mode
           material-resource-alpha-cutoff))
