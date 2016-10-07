(in-package :cl-bodge.definition)


(defpackage :cl-bodge.utils
  (:nicknames :bge.util)
  (:use :cl :alexandria)
  (:export log-errors
           with-hash-entries
           make-hash-table-with-entries
           stream->byte-array
           file->byte-array))


(defpackage :cl-bodge.concurrency
  (:nicknames :bge.muth)
  (:use :cl :alexandria :bordeaux-threads :cl-muth)
  (:export make-job-queue
           push-job
           push-body-into
           drain))


(defpackage :cl-bodge.math
  (:nicknames :bge.math)
  (:use :cl :alexandria)
  (:export vec
           vec2
           vec3
           vec4
           vector->array
           makke-vec3
           sequence->vec3

           matrix
           mat4
           matrix->array
           identity-matrix
           rotation-matrix
           translation-matrix
           scaling-matrix
           perspective-projection-matrix
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

           thread-bound-system
           initialize-system
           discard-system
           make-system-context
           destroy-system-context
           execute-looping-action
           continue-looping-action
           start-system-loop
           execute-in-system-thread
           with-system-context
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
           subscribe-to))


(defpackage :cl-bodge.application
  (:nicknames :bge.app)
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.concurrency :cl-bodge.event
        :cl :bordeaux-threads :alexandria :cl-muth :trivial-main-thread)
  (:export application-system
           
           bind-rendering-context
           swap-buffers

           keyboard-event
           mouse-event
           framebuffer-size-change-event))


(defpackage :cl-bodge.graphics
  (:nicknames :bge.gx)
  (:use :cl-bodge.engine :cl-bodge.application :cl-bodge.concurrency :cl-bodge.utils
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
  (:use :cl-bodge.engine 
        :cl :alexandria)
  (:nicknames :bge.phx)
  (:export physics-system))


(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))
