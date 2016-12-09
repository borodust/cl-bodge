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


(defpackage :cl-bodge.concurrency.generated
  (:nicknames :ge.mt.gen))


(defpackage :cl-bodge.concurrency
  (:nicknames :ge.mt)
  (:use :cl-bodge.utils :cl-bodge.memory
        :cl :bordeaux-threads :cl-muth)
  (:export make-job-queue
           push-job
           push-body-into
           drain
           clearup

           execute
           make-simple-executor
           make-single-threaded-executor
           make-pooled-executor

           in-new-thread
           in-new-thread-waiting

           ->
           dispatch
           defun/d
           wait-for
           wait-for*))


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
           make-vec3

           mat
           square-mat
           mat2
           mat3
           mat4
           square-matrix-size
           mref
           mat->array
           make-mat3
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
           perspective-projection-mat
           orthographic-projection-mat

           quat
           identity-quat
           sequence->quat
           euler-axis>-quat
           euler-angles->quat
           quat->rotation-mat3
           quat->rotation-mat4))


(ge.util:reexporting (:cl-bodge.concurrency :cl-bodge.memory :cl-bodge.math) :cl-bodge.engine

  (defpackage :cl-bodge.engine
    (:nicknames :ge.ng)
    (:use :cl-bodge.utils :cl :bordeaux-threads :cl-muth)
    (:export system
             enable
             disable
             enabledp
             acquire-executor
             release-executor
             working-directory
             merge-working-pathname

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
             check-system-context
             thread-bound-object
             define-system-function

             engine-system
             engine
             property
             startup
             shutdown)))
