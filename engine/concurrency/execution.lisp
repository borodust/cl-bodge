(in-package :cl-bodge.concurrency)


(define-constant +default-queue-size+ 256)


(define-constant +default-pool-size+ 4)


(defgeneric execute (executor task &optional priority))


(defclass simple-executor (disposable)
  ((queue :initform nil :reader task-queue-of)))


(defmethod initialize-instance :after ((this simple-executor) &key queue-size)
  (with-slots (queue) this
    (setf queue (make-blocking-queue queue-size))))


(define-destructor simple-executor (queue)
  (interrupt queue))


(definline make-simple-executor (&optional queue-size)
  (make-instance 'simple-executor :queue-size queue-size))


(defun run (executor)
  (block interruptible
    (loop
       (block continued
         (handler-bind ((interrupted (lambda (e)
                                       (declare (ignore e))
                                       (return-from interruptible))) ; leave loop
                        (t (lambda (e)
                             (log:error "Uncaught error during task execution: ~a" e)
                             (break)
                             (return-from continued)))) ; continue looping
           (funcall (pop-from (task-queue-of executor))))))))


(defmethod execute ((this simple-executor) (task function) &optional (priority :medium))
  (put-into (task-queue-of this) task priority))


;;;
;;;
;;;
(defclass single-threaded-executor (disposable)
  ((executor :initform (make-simple-executor +default-queue-size+))))


(defmethod initialize-instance :after ((this single-threaded-executor) &key)
  (with-slots (executor) this
    (bt:make-thread (lambda () (run executor)) :name "single-threaded-executor")))


(define-destructor single-threaded-executor (executor)
  (dispose executor))


(definline make-single-threaded-executor ()
  (make-instance 'single-threaded-executor))


(defmethod execute ((this single-threaded-executor) (task function) &optional (priority :medium))
  (with-slots (executor) this
    (execute executor task priority)))

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


(defmethod execute ((this pooled-executor) (task function) &optional (priority :medium))
  (with-slots (pool) this
    (push-to-pool pool task priority)))
