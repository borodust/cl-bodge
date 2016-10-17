(in-package :cl-bodge.engine)


(proclaim '(special *system-context*))


(defclass thread-bound-system (generic-system)
  ((thread :initform nil)
   (job-queue :initform nil :accessor %job-queue-of)
   (context :initform nil)))


(defmethod enabledp ((this thread-bound-system))
  (with-slots (thread) this
    (not (null thread))))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system)
             nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defgeneric start-system-loop (system)
  (:method ((this thread-bound-system))
    (loop while (enabledp this) do
         (handler-case
             (funcall (pop-from (%job-queue-of this)))
           (interrupted ()) ; just continue execution
           (t (e) (log:error "Unexpected error during task execution: ~a" e))))))


(defmethod execute ((this thread-bound-system) fn)
  (with-system-lock-held (this)
    (unless (enabledp this)
      (error "Can't execute tasks. System ~a disabled" (class-name (class-of this))))
    (with-promise (resolve reject)
      (handler-case
          (put-into (%job-queue-of this)
                    (lambda ()
                      (handler-case
                          (resolve (funcall fn))
                        (t (e) (log:error e) (reject e)))))
        (interrupted ()))))) ; just continue execution


(defmethod enable ((this thread-bound-system))
  (with-slots (thread) this
    (let ((system-class-name (class-name (class-of this))))
      (when (enabledp this)
        (error "~a already enabled" system-class-name))
      (wait-with-latch (latch)
        (bt:make-thread
         (lambda ()
           (log-errors
             (unwind-protect
                  (progn
                    (with-system-lock-held (this)
                      (setf (%job-queue-of this) (make-blocking-queue 512))
                      (initialize-system this)
                      (setf thread (current-thread)))
                    (open-latch latch)
                    (log:debug "Starting ~a loop" system-class-name)
                    (let ((*system-context* (make-system-context this)))
                      (declare (special *system-context*))
                      (unwind-protect
                           (start-system-loop this)
                        (destroy-system-context *system-context* this))))
               (open-latch latch)
               (log:debug "~a loop stopped" system-class-name)
               (discard-system this))))
         :name (format nil "~a-worker" (string-downcase (string system-class-name))))))))


(defmethod disable ((this thread-bound-system))
  (with-slots (thread) this
    (let ((system-thread thread))
      (with-system-lock-held (this)
        (unless (enabledp this)
          (error "~a already disabled" (class-name (class-of this))))
        (setf thread nil)
        (interrupt (%job-queue-of this)))
      (join-thread system-thread))))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))


;;
(defclass thread-bound-object (system-object) ())


(defmethod execute ((this thread-bound-object) fn)
  (execute (system-of this) fn))
