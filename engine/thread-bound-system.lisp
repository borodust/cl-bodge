(cl:in-package :cl-bodge.engine)


(declaim (special *system-context*))


(defclass thread-bound-system (enableable dispatching generic-system)
  ((executor :initform nil :accessor %executor-of)
   (context :initform nil :reader system-context-of))
  (:documentation "Base class for systems that bound to single thread: either by underlying
 requirements (stateful thread-unsafe foreign systems (e.g. OpenGL contexts) or as a way to
 handle concurrency."))


(defgeneric make-system-context (system)
  (:documentation "Make context bound to system's thread.")
  (:method (system)
    (declare (ignore system)) nil))


(defgeneric destroy-system-context (context system)
  (:documentation "Destroy system context in the thread it was bound to.")
  (:method (context system) (declare (ignore context system))))


(defmethod dispatch ((this thread-bound-system) fn invariant &key (priority :medium)
                                                               (important-p t))
  "Dispatch task to be executed in the system's thread."
  (flet ((invoker ()
           (let ((*system-context* (system-context-of this))
                 (*system* this))
             (funcall fn))))
    (execute (%executor-of this) #'invoker :priority priority :important-p important-p)))


(defgeneric acquire-system-executor (system)
  (:documentation "Acquire executor to use in the system as a dispatch target. Engine's single
 threaded non-exclusive (shared) executor is acquired by default.")
  (:method ((this thread-bound-system))
    (acquire-executor :single-threaded-p t)))


(defgeneric release-system-executor (system executor)
  (:documentation "Release system's executor acquired earlier.")
  (:method ((this thread-bound-system) executor)
    (release-executor executor)))


(defmethod enabling-flow list ((this thread-bound-system))
  (with-slots (context) this
    (%> ()
      (setf (%executor-of this) (acquire-system-executor this))
      (labels ((%fire-error ()
                 (interrupt-flow "Failed to create a context"))
               (%continue-async ()
                 (run (concurrently ()
                        (continue-flow))))
               (%create-context ()
                 (bind-for-serious-condition (#'%fire-error)
                   (setf context (make-system-context this)))
                 (%continue-async)))
        (execute (%executor-of this) #'%create-context
                 :priority :highest :important-p t)))))


(defmethod disabling-flow list ((this thread-bound-system))
  (%> ()
    (flet ((%continue-async ()
             (run (concurrently ()
                    (continue-flow)))))
      (if (alivep (%executor-of this))
          (execute (%executor-of this)
                   (lambda ()
                     (unwind-protect
                          (destroy-system-context this (system-context-of this))
                       (%continue-async)))
                   :priority :highest :important-p t)
          (%continue-async)))
    (instantly ()
      (release-system-executor this (%executor-of this)))))
