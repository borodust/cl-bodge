(ge.util:define-package :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :static-vectors)
  (:export engine-resource-name

           encode-resource
           decode-resource
           handler-resource-type

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
           make-image-resource-handler

           mount-container

           ;; audio
           load-ogg-vorbis-audio

           ;; images
           load-png-image

           ;; chunked
           write-chunk
           write-brf-magic

           ;; ttf font
           define-sdf-font
           sdf-font-atlas-resource-name
           sdf-font-metrics-resource-name
           make-font-atlas-resource
           font-atlas-resource-ascender
           font-atlas-resource-descender
           font-atlas-resource-line-gap
           font-atlas-resource-glyphs
           make-glyph-metrics-resource
           glyph-metrics-resource-character
           glyph-metrics-resource-origin
           glyph-metrics-resource-bounding-box
           glyph-metrics-resource-advance-width
           glyph-metrics-resource-kernings

           ;; ttf font
           font-container-data

           ;; scene
           make-empty-scene-resource
           do-scene-resource-materials
           do-scene-resource-meshes
           scene-resource-mesh
           scene-resource-material
           ;; mesh
           make-mesh-resource
           mesh-resource-position-array
           mesh-resource-index-array
           mesh-resource-normal-array
           mesh-resource-tangent-array
           mesh-resource-primitive
           mesh-resource-color-array
           mesh-resource-tex-coord-array
           do-mesh-resource-color-arrays
           do-mesh-resource-tex-coord-arrays
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
           do-material-resource-textures
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
