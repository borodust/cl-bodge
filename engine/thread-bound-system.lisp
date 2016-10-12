(in-package :cl-bodge.engine)


(proclaim '(special *system-context*))


(defclass thread-bound-system (generic-system)
  ((thread :initform nil)
   (job-queue :initform (make-job-queue) :reader job-queue-of)
   (context :initform nil)))


(defmethod enabledp ((this thread-bound-system))
  (with-slots (thread state-lock) this
    (with-recursive-lock-held (state-lock)
      (not (null thread)))))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system)
             nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defgeneric execute-looping-action (system)
  (:method (system) (declare (ignore system))))


(defgeneric continue-looping-action (system)
  (:method (system) (declare (ignore system))))


(defgeneric start-system-loop (system)
  (:method ((this thread-bound-system))
    (loop while (enabledp this) do
         (log-errors
           (execute-looping-action this))
         (log-errors
           (drain (job-queue-of this))))))


(defmethod execute ((this thread-bound-system) fn)
  (with-system-lock-held (this)
    (with-promise (resolve reject)
      (push-job (lambda ()
                  (handler-case
                      (resolve (funcall fn))
                    (t (e) (reject e))))
                (job-queue-of this))
      (continue-looping-action this))))


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
        (-> this
          (setf thread nil))
        (continue-looping-action this))
      (join-thread system-thread))))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))
