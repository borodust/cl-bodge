(in-package :cl-bodge.engine)


(defclass thread-bound-system (system)
  ((thread :initform nil)
   (job-queue :initform (make-job-queue) :reader job-queue-of)
   (context :initform nil)
   (state-lock :initform (make-recursive-lock "tb-sys-state-lock"))
   (state-condi-var :initform (make-condition-variable
                               :name "tb-sys-state-condi-var"))))

(declaim (inline %tbs-enabled-p))
(defun %tbs-enabled-p (system)
  (with-slots (thread) system
    (not (null thread))))


(defgeneric initialize-system (system)
  (:method (system) (declare (ignore system))))


(defgeneric discard-system (system)
  (:method (system) (declare (ignore system))))


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
    (loop while (%tbs-enabled-p this) do
         (log-errors
           (execute-looping-action this))
         (log-errors
           (drain (job-queue-of this))))))


(defun execute-in-system-thread (system fn)
  (push-job fn (job-queue-of system))
  (continue-looping-action system)
  nil)


(defmacro with-system-context ((&optional (ctx-var (gensym "ctx"))
                                          (sys-var (gensym "sys"))) sys
                               &body body)
  (once-only (sys)
    `(execute-in-system-thread ,sys
                               (lambda ()
                                 (declare (special *system-context*))
                                 (let ((,ctx-var *system-context*)
                                       (,sys-var ,sys))
                                   (declare (ignorable ,ctx-var ,sys-var))
                                   ,@body)))))


(defmethod enable ((this thread-bound-system))
  (with-slots (thread state-lock) this
    (let ((system-class-name (class-name (class-of this))))
      (with-recursive-lock-held (state-lock)
        (when (%tbs-enabled-p this)
          (error "~a already enabled" system-class-name)))
      (wait-with-latch (latch)
        (bt:make-thread
         (lambda ()
           (log-errors
             (unwind-protect
                  (progn
                    (with-recursive-lock-held (state-lock)
                      (initialize-system this)
                      (setf thread (current-thread)))
                    (open-latch latch)
                    (log:trace "Starting ~a loop" system-class-name)
                    (let ((*system-context* (make-system-context this)))
                      (declare (special *system-context*))
                      (unwind-protect
                           (start-system-loop this)
                        (destroy-system-context *system-context* this))))
               (open-latch latch)
               (log:trace "~a loop stopped" system-class-name)
               (discard-system this))))
         :name (format nil "~a-worker" (string-downcase (string system-class-name))))))))


(defmethod disable ((this thread-bound-system))
  (with-slots (thread state-lock) this
    (let ((system-thread thread))
      (with-recursive-lock-held (state-lock)
        (unless (%tbs-enabled-p this)
          (error "~a already disabled" (class-name (class-of this))))
        (with-system-context () this
          (setf thread nil))
        (continue-looping-action this))
      (join-thread system-thread))))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))
