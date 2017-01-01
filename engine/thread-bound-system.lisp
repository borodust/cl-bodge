(in-package :cl-bodge.engine)


(declaim (special *system-context*))


(defclass thread-bound-system (enableable dispatcher generic-system)
  ((executor :initform nil :accessor %executor-of)
   (context :initform nil :reader system-context-of)))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system) nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defmethod dispatch ((this thread-bound-system) fn &key (priority :medium) important-p)
  (unless (call-next-method)
    (flet ((invoker ()
             (log-errors
               (let ((*system-context* (system-context-of this))
                     (*system* this))
                 (funcall fn)))))
      (execute (%executor-of this) #'invoker :priority priority :important-p important-p))
    t))


(defgeneric acquire-system-executor (system)
  (:method ((this thread-bound-system))
    (acquire-executor :single-threaded-p t :exclusive-p t)))


(defgeneric release-system-executor (system executor)
  (:method ((this thread-bound-system) executor)
    (release-executor executor)))


(defmethod initialize-system :after ((this thread-bound-system))
  (setf (%executor-of this) (acquire-system-executor this)))


(defmethod initialize-system :around ((this thread-bound-system))
  (call-next-method)
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (log-errors
                 (with-slots (context) this
                   (setf context (make-system-context this)))
                 (open-latch latch)))
             :priority :highest :important-p t)))


(defmethod discard-system :around ((this thread-bound-system))
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (unwind-protect
                    (destroy-system-context this (system-context-of this))
                 (open-latch latch)))
             :priority :highest :important-p t))
  (call-next-method))


(defmethod discard-system :before ((this thread-bound-system))
  (release-system-executor this (%executor-of this)))
