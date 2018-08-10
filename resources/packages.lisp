(ge.util:define-package :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :static-vectors)
  (:export engine-resource-name

           encode-resource
           decode-resource
           handler-resource-type
           resource-dependencies

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
           scene-resource-root-node
           do-scene-resource-materials
           do-scene-resource-meshes
           do-scene-resource-animations
           scene-resource-mesh
           scene-resource-material
           scene-resource-animation
           ;; mesh
           make-mesh-resource
           mesh-resource-primitive
           mesh-resource-position-array
           mesh-resource-index-array
           mesh-resource-normal-array
           mesh-resource-tangent-array
           mesh-resource-bone-id-array
           mesh-resource-bone-weight-array
           mesh-resource-color-array
           mesh-resource-tex-coord-array
           add-mesh-resource-bone
           mesh-resource-bone
           mesh-resource-bone-count
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
           texture-resource-uv-transform
           texture-resource-scale
           texture-resource-strength
           ;; material
           make-material-resource
           material-resource-texture
           do-material-resource-textures
           material-resource-name
           material-resource-shininess
           material-resource-shininess-strength
           material-resource-opacity
           material-resource-reflectivity
           material-resource-bump-scaling
           material-resource-displacement-scaling
           material-resource-specular-color
           material-resource-diffuse-color
           material-resource-ambient-color
           material-resource-emissive-color
           material-resource-transparent-color
           material-resource-reflective-color
           material-resource-emissive-color
           material-resource-base-color-factor
           material-resource-metallic-factor
           material-resource-roughness-factor
           material-resource-glossiness-factor
           material-resource-alpha-mode
           material-resource-alpha-cutoff
           ;; animations
           make-animation-resource
           animation-resource-duration
           add-animation-resource-channel
           animation-resource-channels
           make-animation-resource-channel
           animation-resource-channel-node-id
           animation-resource-channel-pre-state
           animation-resource-channel-post-state
           animation-resource-channel-rotation-sequence
           animation-resource-channel-translation-sequence
           animation-resource-channel-rotation-sequence
           make-animation-resource-key-sequence
           animation-resource-key-sequence-timing-array
           animation-resource-key-sequence-value-array
           ;; bones
           make-mesh-resource-bone
           mesh-resource-bone-offset
           ;; nodes
           make-scene-resource-node
           scene-resource-node-id
           scene-resource-node-transform
           scene-resource-node-mesh-id-list
           scene-resource-node-property
           add-scene-resource-node-child
           add-scene-resource-node-mesh-id))
