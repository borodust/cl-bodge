(in-package :cl-bodge.engine)


(proclaim '(special *system-context*))


(defclass thread-bound-system (generic-system)
  ((thread :initform nil)
   (job-queue :initform (make-job-queue) :reader %job-queue-of)
   (loop-lock :initform (make-lock "tbs-loop-lock"))
   (loop-condition :initform (make-condition-variable :name "tbs-loop-condition"))
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


(defgeneric continue-looping (system)
  (:method ((this thread-bound-system))
    (with-slots (loop-condition) this
      (condition-notify loop-condition))))


(defgeneric start-system-loop (system)
  (:method ((this thread-bound-system))
    (with-slots (loop-lock loop-condition job-queue) this
     (with-lock-held (loop-lock)
       (loop while (enabledp this)
          with i = 0
          for count = (drain (%job-queue-of this))
          when (= 0 count) do (incf i) else do (setf i 0)
          when (> i 1000) do (condition-wait loop-condition loop-lock))))))


(defmethod execute ((this thread-bound-system) fn)
  (with-system-lock-held (this)
    (with-promise (resolve reject)
      (push-job (lambda ()
                  (handler-case
                      (resolve (funcall fn))
                    (t (e) (log:error e) (reject e))))
                (%job-queue-of this))
      (continue-looping this))))


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
          (setf thread nil)))
      (join-thread system-thread))))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))


;;
(defclass thread-bound-object (system-object) ())


(defmethod execute ((this thread-bound-object) fn)
  (execute (system-of this) fn))
