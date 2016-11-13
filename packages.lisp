(in-package :cl-bodge.definition)


(defpackage :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :local-time :alexandria)
  (:export with-gensyms
           once-only
           symbolicate
           make-keyword
           when-let
           when-let*
           if-let
           switch
           define-constant
           alist-hash-table
           read-file-into-string
           nconcf
           starts-with-subseq
           positive-integer
           copy-array
           deletef
           alist-hash-table
           ensure-list

           log-errors
           with-hash-entries
           make-hash-table-with-entries
           stream->byte-array
           file->byte-array
           defenum
           f
           epoch-seconds
           definline
           copy-memory

           ensure-not-null
           if-unbound
           when-bound

           class-name-of
           dolines

           parent
           adopt
           abandon
           dochildren
           children-of
           dotree

           search-sorted
           list->array))


(defpackage :cl-bodge.concurrency
  (:nicknames :ge.mt)
  (:use :cl-bodge.utils
        :cl :bordeaux-threads :cl-muth)
  (:import-from :blackbird
                promisep
                promise-finished-p
                create-promise
                with-promise
                promisify
                attach
                catcher
                tap
                finally
                alet
                alet*
                aif
                multiple-promise-bind
                all)
  (:export make-job-queue
           push-job
           push-body-into
           drain
           clearup

           ->
           execute

           promisep
           promise-finished-p
           create-promise
           with-promise
           promisify
           attach
           catcher
           tap
           finally
           when-all
           when-all*
           wait-let
           wait
           alet
           alet*
           aif
           multiple-promise-bind
           all))


(defpackage :cl-bodge.math
  (:nicknames :ge.math)
  (:use :cl :cl-bodge.utils)
  (:export lerp
           nlerp
           mult
           sum
           div
           subt
           normalize
           inverse

           vec
           vec2
           vec3
           vec4
           vec->array
           sequence->vec3
           sequence->vec4
           sequence->vec3
           vref

           mat
           square-mat
           mat2
           mat3
           mat4
           square-matrix-size
           mref
           mat->array
           identity-mat4
           sequence->mat4
           sequence->rotation-mat4
           euler-axis->mat4
           euler-angles->mat4
           translation-mat4
           sequence->translation-mat4
           vec->translation-mat4
           scaling-mat4
           vec->scaling-mat4
           mat4->mat3
           perspective-projection-mat

           quat
           identity-quat
           sequence->quat
           euler-axis>-quat
           euler-angles->quat
           quat->rotation-mat3
           quat->rotation-mat4))


(defpackage :cl-bodge.memory
  (:nicknames :ge.mem)
  (:use :cl-bodge.utils
        :cl :trivial-garbage)
  (:export define-destructor
           dispose
           disposable
           disposable-container
           with-disposable))



(defpackage :cl-bodge.engine
  (:nicknames :ge.ng)
  (:use :cl-bodge.utils :cl-bodge.concurrency
        :cl :bordeaux-threads :cl-muth)
  (:export system
           enable
           disable
           enabledp
           system-object
           system-of
           enableable

           generic-system
           with-system-lock-held
           initialize-system
           discard-system

           thread-bound-system
           make-system-context
           destroy-system-context
           start-system-loop
           *system-context*
           check-system-context
           thread-bound-object

           engine-system
           property
           startup
           shutdown))

(defpackage :cl-bodge.event
  (:nicknames :ge.eve)
  (:use :cl-bodge.engine :cl-bodge.utils
        :cl :bordeaux-threads :cl-muth)
  (:export event-system
           event
           defevent
           register-event-class
           register-event-classes
           post
           subscribe-to
           subscribe-with-handler-body-to))


(defpackage :cl-bodge.host
  (:nicknames :ge.host)
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.concurrency :cl-bodge.event
        :cl :bordeaux-threads :cl-muth :trivial-main-thread)
  (:export host-system

           bind-rendering-context
           swap-buffers

           state-from
           keyboard-event
           key-from
           mouse-event
           button-from
           cursor-event
           x-from
           y-from
           scroll-event
           x-offset-from
           y-offset-from
           framebuffer-size-change-event
           width-from
           height-from))


(defpackage :cl-bodge.graphics.resources
  (:nicknames :ge.gx.rsc)
  (:use :cl-bodge.utils
        :cl)
  (:export pixel-format

           pixel-format-of
           image->array
           size-of

           shader-text-of
           shader-type-of
           shader-name-of
           reload-shader-text))


(defpackage :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl-bodge.engine :cl-bodge.host :cl-bodge.concurrency :cl-bodge.utils
        :cl-bodge.math :cl-bodge.event :cl-bodge.memory :cl-bodge.graphics.resources
        :cl :cl-muth :bordeaux-threads)
  (:export graphics-system
           in-wireframe-mode

           render

           make-vertex-array

           attach-gpu-buffer
           make-array-buffer
           make-index-buffer

           primitive
           make-mesh
           make-patch-mesh

           compile-shader
           make-shading-program
           make-separable-shading-program
           link-separable-shading-program
           build-separable-shading-program

           use-shading-program
           program-uniform-variable

           with-bound-texture
           make-2d-texture

           make-shading-pipeline
           use-shading-program-stages
           with-bound-shading-pipeline))


(defpackage :cl-bodge.audio.resources
  (:nicknames :ge.snd.rsc)
  (:use :cl-bodge.utils
        :cl)
  (:export pcm-data
           sample-depth
           channel-format

           pcm-audio-data-of
           audio-channel-format-of
           audio-sample-depth-of
           audio-sampling-rate-of))


(defpackage :cl-bodge.audio
  (:use :cl-bodge.engine :cl-bodge.math :cl-bodge.memory :cl-bodge.concurrency
        :cl-bodge.utils :cl-bodge.audio.resources
        :cl :cl-muth)
  (:nicknames :ge.snd)
  (:export audio-system

           listener-gain
           listener-position
           listener-velocity
           listener-orientation

           make-audio-source
           play-audio
           pause-audio
           stop-audio

           make-audio-buffer
           attach-audio-buffer))


(defpackage :cl-bodge.physics
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.math :cl-bodge.memory
        :cl-bodge.concurrency :cl-bodge.utils
        :cl :local-time)
  (:nicknames :ge.phx)
  (:export physics-system
           observe-universe

           make-rigid-body
           position-of
           rotation-of
           linear-velocity-of
           angular-velocity-of
           mass-of

           make-ball-joint
           make-hinge-joint
           make-slider-joint
           make-universal-joint
           make-double-hinge-joint
           make-angular-motor-joint

           make-sphere-geom
           make-box-geom
           make-plane-geom
           make-capped-cylinder-geom
           make-ray-geom
           bind-geom

           make-box-mass))


(defpackage :cl-bodge.animation
  (:nicknames :ge.ani)
  (:use :cl-bodge.utils :cl-bodge.math
        :cl)
  (:export make-keyframe
           make-keyframe-sequence
           transform-at
           make-keyframe-animation
           frame-at
           frame-transform-of
           start-animation
           reset-animation
           keyframed))


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


(defpackage :cl-bodge.scene
  (:nicknames :ge.sg)
  (:use :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics :cl-bodge.physics
        :cl-bodge.math :cl-bodge.concurrency :cl-bodge.host :cl-bodge.memory
        :cl-bodge.animation :cl-bodge.resources
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


(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))
