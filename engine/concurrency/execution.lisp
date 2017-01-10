(in-package :cl-bodge.concurrency)


(define-constant +default-queue-size+ 1024)


(define-constant +default-pool-size+ 4)


(defgeneric execute (executor task &key &allow-other-keys))


(defclass generic-executor (disposable)
  ((queue :initform nil :reader task-queue-of)))


(defmethod initialize-instance :after ((this generic-executor) &key queue-size)
  (with-slots (queue) this
    (setf queue (make-blocking-queue queue-size))))


(define-destructor generic-executor (queue)
  (interrupt queue))


(defun run (executor)
  (block interruptible
    (loop
       (block continued
         (handler-bind ((interrupted (lambda (e)
                                       (declare (ignore e))
                                       (return-from interruptible))) ; leave loop
                        (t (lambda (e)
                             (log:error "Uncaught error during task execution: ~a" e)
                             (when-debugging
                               (break))
                             (return-from continued)))) ; continue looping
           (funcall (pop-from (task-queue-of executor))))))))


(defclass blocking-executor (generic-executor) ())

(defmethod execute ((this blocking-executor) (task function) &key (priority :medium))
  (put-into (task-queue-of this) task priority))

(definline make-blocking-executor (&optional queue-size)
  (make-instance 'blocking-executor :queue-size queue-size))


(defclass discarding-executor (generic-executor) ())

(defmethod execute ((this discarding-executor) (task function) &key
                                                                 (priority :medium)
                                                                 (important-p t))
  (if important-p
      (put-into (task-queue-of this) task priority)
      (try-put-replacing (task-queue-of this) task priority)))


(definline make-discarding-executor (&optional queue-size)
  (make-instance 'discarding-executor :queue-size queue-size))


;;;
;;;
;;;
(defclass single-threaded-executor (disposable)
  ((executor :initform (make-discarding-executor +default-queue-size+))))


(defmethod initialize-instance :after ((this single-threaded-executor) &key special-variables)
  (with-slots (executor) this
    (bt:make-thread (lambda ()
                      (progv special-variables (mapcar (constantly nil) special-variables)
                        (run executor)))
                    :name "single-threaded-executor")))


(define-destructor single-threaded-executor (executor)
  (dispose executor))


(definline make-single-threaded-executor (&optional special-variables)
  (make-instance 'single-threaded-executor :special-variables special-variables))


(defmethod execute ((this single-threaded-executor) (task function) &key
                                                                      (priority :medium)
                                                                      (important-p t))
  (with-slots (executor) this
    (execute executor task :priority priority :important-p important-p)))

;;;
;;;
;;;
(defclass pooled-executor (disposable)
  ((pool :initform nil)))


(defmethod initialize-instance :after ((this pooled-executor) &key (size +default-pool-size+))
  (with-slots (pool) this
    (setf pool (make-thread-pool size))
    (open-pool pool "pooled-executor-worker")))


(define-destructor pooled-executor (pool)
  (close-pool pool))


(definline make-pooled-executor (&optional (size +default-pool-size+))
  (make-instance 'pooled-executor :size size))


(defmethod execute ((this pooled-executor) (task function) &key (priority :medium))
  (with-slots (pool) this
    (push-to-pool pool task priority)))
