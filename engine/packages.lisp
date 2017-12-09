(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.memory
  (:nicknames :ge.mem)
  (:use :cl :cl-bodge.utils :trivial-garbage :static-vectors)
  (:export *auto-initialize-destructor*

           define-destructor
           initialize-destructor
           dispose
           disposable
           disposable-container
           with-disposable

           make-foreign-array
           simple-array-of
           foreign-pointer-of))


(ge.util:define-package :cl-bodge.concurrency
  (:nicknames :ge.mt)
  (:use :cl-bodge.utils :cl-bodge.memory :cl :bordeaux-threads
        :cl-muth :cl-flow)
  (:export make-task-queue
           push-task
           push-body-into
           drain
           clearup

           make-guarded-reference
           guarded-value-of
           with-guarded-reference

           execute
           make-single-threaded-executor
           make-pooled-executor

           in-new-thread
           in-new-thread-waiting
           with-body-in-main-thread
           stop-main-runner

           ->
           >>
           %>
           ~>
           ->>
           continue-flow
           interrupt-flow
           dispatch

           lockable
           with-instance-lock-held))


(ge.util:define-package :cl-bodge.math
  (:nicknames :ge.math)
  (:use :cl :cl-bodge.utils)
  (:export lerp
           nlerp
           mult
           add
           div
           subt
           cross
           dot
           normalize
           inverse

           transform-of

           vec
           vec2
           vec3
           vec4
           vec->array
           sequence->vec2
           sequence->vec3
           sequence->vec4
           vref
           make-vec3
           make-vec4
           x
           y
           z
           w
           vector-length

           mat
           square-mat
           mat2
           mat3
           mat4
           square-matrix-size
           mref
           mat->array
           make-mat3
           identity-mat3
           identity-mat4
           sequence->mat4
           sequence->rotation-mat4
           mat->rotation-mat4
           euler-axis->mat4
           angle->mat2
           euler-angles->mat4
           euler-angles->mat3
           rotation-translation->mat4
           translation-mat4
           sequence->translation-mat4
           vec->translation-mat4
           scaling-mat4
           vec->scaling-mat4
           mat4->mat3
           mat3->mat4
           basis->mat4
           perspective-projection-mat
           orthographic-projection-mat

           quat
           identity-quat
           sequence->quat
           euler-axis->quat
           euler-angles->quat
           quat->rotation-mat3
           quat->rotation-mat4
           rotate))


(ge.util:define-package :cl-bodge.engine.resources
  (:nicknames :ge.ng.rsc)
  (:use :cl :cl-bodge.utils)
  (:export pixel-format
           pixel-format-p

           pixel-format-of
           foreign-array-of
           width-of
           height-of

           pcm-data
           sample-depth
           channel-format

           pcm-audio-data-of
           audio-channel-format-of
           audio-sample-depth-of
           audio-sampling-rate-of))


(ge.util:define-package :cl-bodge.events
  (:nicknames :ge.eve)
  (:use :cl :cl-bodge.utils :cl-bodge.concurrency :cl-bodge.memory)
  (:export event
           defevent
           event-emitting
           fire-event
           subscribe-to
           unsubscribe-from
           subscribe-body-to
           event-listening
           register-event-handler
           deregister-event-handler
           deregister-by-event-emitter
           subscribe-listener
           unsubscribe-listener
           make-event-hub
           register-event-emitter
           deregister-event-emitter
           enable-hub
           disable-hub))


(ge.util:define-package :cl-bodge.engine
  (:nicknames :ge.ng)
  (:use :cl-bodge.utils :cl :bordeaux-threads :cl-muth :cl-bodge.events)
  (:use-reexport :cl-bodge.concurrency :cl-bodge.memory :cl-bodge.math
                 :cl-bodge.engine.resources)
  (:export system
           enabling-flow
           disabling-flow
           enabledp
           acquire-executor
           release-executor
           working-directory
           merge-working-pathname

           dispatching
           instantly
           concurrently
           value-flow
           null-flow
           assembly-flow
           initialization-flow
           run

           system-object
           system-of
           enableable

           handle-value-of
           defhandle
           *handle-value*
           value-of

           foreign-object
           handle-of
           system-foreign-object

           generic-system
           with-system-lock-held
           initialize-system
           discard-system

           thread-bound-system
           system-context-of
           make-system-context
           destroy-system-context
           *system-context*
           *system*
           define-system-function
           acquire-system-executor
           release-system-executor

           *engine-startup-hooks*
           after-system-startup
           before-system-shutdown
           engine-system
           engine
           executablep
           property
           startup
           shutdown

           defevent
           post
           subscribe
           subscribe-body
           unsubscribe
           define-event-handler
           subscribing
           add-event-handler
           remove-event-handler
           employ-subscriber
           dismiss-subscriber))
