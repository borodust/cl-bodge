(in-package :cl-bodge.definition)


(defpackage :cl-bodge.utils
  (:nicknames :bge.util)
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
           copy-memory))


(defpackage :cl-bodge.concurrency
  (:nicknames :bge.mt)
  (:use :cl :alexandria :bordeaux-threads :cl-muth :blackbird)
  (:export make-job-queue
           push-job
           push-body-into
           drain

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
           alet
           alet*
           aif
           multiple-promise-bind
           all))


(defpackage :cl-bodge.math
  (:nicknames :bge.math)
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


(defpackage :cl-bodge.resources
  (:nicknames :bge.rsc)
  (:use :cl :alexandria)
  (:export read-file-into-string-list

           load-shader-source
           shader-type-of
           shader-text-of))


(defpackage :cl-bodge.engine
  (:nicknames :bge.ng)
  (:use :cl-bodge.utils :cl-bodge.concurrency
        :cl :alexandria :bordeaux-threads :cl-muth)
  (:export system
           enable
           disable
           enabledp

           generic-system
           with-system-lock-held
           initialize-system
           discard-system

           thread-bound-system
           make-system-context
           destroy-system-context
           execute-looping-action
           continue-looping-action
           start-system-loop
           *system-context*
           check-system-context

           engine-system
           property
           startup
           shutdown))

(defpackage :cl-bodge.event
  (:nicknames :bge.eve)
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
  (:nicknames :bge.host)
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


(defpackage :cl-bodge.graphics
  (:nicknames :bge.gx)
  (:use :cl-bodge.engine :cl-bodge.host :cl-bodge.concurrency :cl-bodge.utils
        :cl-bodge.math :cl-bodge.event :cl-bodge.resources
        :cl :alexandria :cl-muth :bordeaux-threads)
  (:export graphics-system
           render-scene
           within-rendering-context

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
           program-uniform-variable))


(defpackage :cl-bodge.audio
  (:use :cl-bodge.engine :cl-bodge.math
        :cl :alexandria :cl-muth)
  (:nicknames :bge.snd)
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
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.math
        :cl :alexandria :local-time)
  (:nicknames :bge.phx)
  (:export physics-system

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


(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))
