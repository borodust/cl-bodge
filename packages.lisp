(in-package :cl-bodge.definition)


(defpackage :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :alexandria :local-time)
  (:export log-errors
           with-hash-entries
           make-hash-table-with-entries
           stream->byte-array
           file->byte-array
           defenum
           f
           epoch-seconds-of
           definline
           copy-memory

           ensure-not-null))


(defpackage :cl-bodge.concurrency
  (:nicknames :ge.mt)
  (:use :cl :alexandria :bordeaux-threads :cl-muth :blackbird)
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
           alet
           alet*
           aif
           multiple-promise-bind
           all))


(defpackage :cl-bodge.math
  (:nicknames :ge.math)
  (:use :cl :alexandria :cl-bodge.utils)
  (:export vec
           vec2
           vec3
           vec4
           vec->array
           make-vec3
           sequence->vec3
           vref

           mat
           mat4
           mref
           mat->array
           identity-mat4
           rotation-mat4
           rotation-mat4*
           translation-mat4
           translation-mat4*
           scaling-mat4*
           perspective-projection-mat
           m*))


(defpackage :cl-bodge.memory
  (:nicknames :ge.mem)
  (:use :cl-bodge.utils
        :cl :alexandria :trivial-garbage)
  (:export define-destructor
           dispose
           disposable
           disposable-container
           with-disposable))



(defpackage :cl-bodge.engine
  (:nicknames :ge.ng)
  (:use :cl-bodge.utils :cl-bodge.concurrency
        :cl :alexandria :bordeaux-threads :cl-muth)
  (:export system
           enable
           disable
           enabledp
           system-object
           system-of

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
        :cl :alexandria :bordeaux-threads :cl-muth)
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
        :cl :bordeaux-threads :alexandria :cl-muth :trivial-main-thread)
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
           shader-type-of))


(defpackage :cl-bodge.graphics
  (:nicknames :ge.gx)
  (:use :cl-bodge.engine :cl-bodge.host :cl-bodge.concurrency :cl-bodge.utils
        :cl-bodge.math :cl-bodge.event :cl-bodge.memory :cl-bodge.graphics.resources
        :cl :alexandria :cl-muth :bordeaux-threads)
  (:export graphics-system

           render
           rendering-group
           add-renderable

           make-vertex-array

           attach-gpu-buffer
           make-array-buffer
           make-index-buffer

           primitive
           make-mesh
           make-indexed-mesh

           make-shading-program
           use-shading-program
           program-uniform-variable

           with-bound-texture
           make-2d-texture))


(defpackage :cl-bodge.audio
  (:use :cl-bodge.engine :cl-bodge.math :cl-bodge.memory :cl-bodge.concurrency
        :cl-bodge.utils
        :cl :alexandria :cl-muth)
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
        :cl :alexandria :local-time)
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


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl-bodge.utils
        :cl :alexandria)
  (:export load-shader-source
           load-png-image))



(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))
