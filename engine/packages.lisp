(ge.util:define-package :cl-bodge.events
  (:nicknames :ge.eve)
  (:use :cl :ge.util :bodge-concurrency :cl-flow :bodge-memory)
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
  (:use-reexport
   :bodge-concurrency
   :bodge-memory
   :bodge-math)
  (:reexport-from :flow
                  #:->
                  #:>>
                  #:~>
                  #:->>
                  #:%>

                  #:continue-flow
                  #:interrupt-flow)
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
           loop-flow
           assembly-flow
           initialization-flow
           run

           system-object
           system-of
           enableable

           handle-value-of
           defhandle
           destroy-handle
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
           dismiss-subscriber

           adopt
           abandon
           abandon-all
           dochildren))
