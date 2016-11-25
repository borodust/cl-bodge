(in-package :cl-bodge.engine)


(declaim (special *system-context*))


(defclass thread-bound-system (enableable generic-system)
  ((executor :initform nil :accessor %executor-of)
   (context :initform nil)))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system)
             nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defmethod dispatch ((this thread-bound-system) fn &optional (priority :medium))
  (execute (%executor-of this) fn priority))


(defun system-class-name-of (this)
  (class-name (class-of this)))


(defmethod enable ((this thread-bound-system))
  (with-system-lock-held (this)
    (when (enabledp this)
      (error "~a already enabled" (system-class-name-of this)))
    (setf (%executor-of this) (acquire-executor :single-threaded-p t :exclusive-p t))
    (initialize-system this)
    (wait-with-latch (latch)
      (execute (%executor-of this)
               (lambda ()
                 (log-errors
                   (defvar *system-context* (make-system-context this))
                   (open-latch latch)))))
    (call-next-method)))


(defmethod disable ((this thread-bound-system))
  (with-system-lock-held (this)
    (unless (enabledp this)
      (error "~a already disabled" (system-class-name-of this)))
    (wait-with-latch (latch)
      (execute (%executor-of this)
               (lambda ()
                 (destroy-system-context *system-context* this)
                 (open-latch latch))))
    (discard-system this)
    (release-executor (%executor-of this))
    (call-next-method)))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))


;;
(defclass thread-bound-object (system-object) ())
