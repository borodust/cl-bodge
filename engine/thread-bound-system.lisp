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


(defmacro %log-unexpected-error (block-name)
  `(lambda (e)
     (log:error "Unexpected error during task execution: ~a" e)
     (return-from ,block-name)))


(defgeneric start-system-loop (system)
  (:method ((this thread-bound-system))
      (loop while (enabledp this) do
           (block interruptible
             (handler-bind ((interrupted (lambda (e)
                                           (declare (ignore e))
                                           (return-from interruptible))) ; leave loop
                            (t (%log-unexpected-error interruptible)))
               (funcall (pop-from (%job-queue-of this))))))))


(defmethod execute ((this thread-bound-system) fn)
  (with-promise (resolve reject)
    (handler-bind ((interrupted (lambda (e)
                                  (declare (ignore e))
                                  (error "Cannot execute task: ~a offline."
                                         (class-name (class-of this)))))
                   (t (lambda (e) (reject e))))
      (let ((task (lambda ()
                    (handler-bind ((t (lambda (e) (log:error "~a" e) (reject e))))
                      (resolve (funcall fn))))))
        (with-slots (thread) this
          (if (eq (bt:current-thread) (with-system-lock-held (this) thread))
              (funcall task)
              (put-into (%job-queue-of this) task)))))))


(defmethod enable ((this thread-bound-system))
  (with-slots (thread) this
    (let ((system-class-name (class-name (class-of this))))
      (with-system-lock-held (this)
        (when (enabledp this)
          (error "~a already enabled" system-class-name))
        (setf (%job-queue-of this) (make-blocking-queue 256))
        (initialize-system this)
        (wait-with-latch (latch)
          (bt:make-thread
           (lambda ()
             (log-errors
               (unwind-protect
                    (progn
                      (setf thread (current-thread))
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
           :name (format nil "~a-worker" (string-downcase (string system-class-name)))))))))


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
