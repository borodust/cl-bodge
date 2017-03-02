(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.memory
  (:nicknames :ge.mem)
  (:use :cl-bodge.utils
        :cl :trivial-garbage)
  (:export define-destructor
           dispose
           disposable
           disposable-container
           with-disposable))


(defpackage :cl-bodge.concurrency
  (:nicknames :ge.mt)
  (:use :cl-bodge.utils :cl-bodge.memory
        :cl :bordeaux-threads :cl-muth :cl-flow)
  (:export make-task-queue
           push-task
           push-body-into
           drain
           clearup

           execute
           make-single-threaded-executor
           make-pooled-executor

           in-new-thread
           in-new-thread-waiting

           ->
           >>
           *>
           ~>
           dispatch
           define-flow

           lockable
           with-instance-lock-held))


(defpackage :cl-bodge.math
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

           vec
           vec2
           vec3
           vec4
           vec->array
           sequence->vec3
           sequence->vec4
           sequence->vec3
           vref
           make-vec3
           make-vec4
           x
           y
           z
           w

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
           euler-angles->mat4
           translation-mat4
           sequence->translation-mat4
           vec->translation-mat4
           scaling-mat4
           vec->scaling-mat4
           mat4->mat3
           basis->mat4
           perspective-projection-mat
           orthographic-projection-mat

           quat
           identity-quat
           sequence->quat
           euler-axis->quat
           euler-angles->quat
           quat->rotation-mat3
           quat->rotation-mat4))


(ge.util:define-package :cl-bodge.engine
  (:nicknames :ge.ng)
  (:use :cl-bodge.utils :cl :bordeaux-threads :cl-muth)
  (:use-reexport :cl-bodge.concurrency :cl-bodge.memory :cl-bodge.math)
  (:export system
           enable
           disable
           enabledp
           notify-system
           acquire-executor
           release-executor
           working-directory
           merge-working-pathname

           dispatcher
           run
           instantly
           concurrently
           value-flow
           null-flow

           system-object
           system-of
           enableable

           foreign-object
           handle-value-of
           destroy-foreign-object
           defhandle
           *handle-value*

           generic-system
           with-system-lock-held
           initialize-system
           discard-system

           thread-bound-system
           make-system-context
           destroy-system-context
           *system-context*
           *system*
           define-system-function
           acquire-system-executor
           release-system-executor


           engine-system
           engine
           property
           startup
           shutdown))
