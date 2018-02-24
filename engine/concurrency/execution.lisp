(cl:in-package :cl-bodge.concurrency)


(define-constant +default-queue-size+ 1024)


(define-constant +default-pool-size+ 4)


(defgeneric execute (executor task &key &allow-other-keys)
  (:documentation "Abstract execution facility: runs `task` in a way defined by
`executor`. Executors must operate in a thread-safe manner."))


(defgeneric alivep (executor)
  (:method (executor) t)
  (:documentation "Checks if executor is alive"))


(defclass generic-executor (disposable)
  ((queue :initform nil :reader task-queue-of)))


(defmethod initialize-instance :after ((this generic-executor) &key queue-size)
  (with-slots (queue) this
    (setf queue (make-blocking-queue queue-size))))


(define-destructor generic-executor (queue)
  (interrupt queue))


(defun ignite (executor)
  (block interruptible
    (loop
       (log-errors
         (handler-bind ((interrupted (lambda (e)
                                       (declare (ignore e))
                                       ;; leave loop
                                       (return-from interruptible))))
           (funcall (pop-from (task-queue-of executor))))))))


(defclass blocking-executor (generic-executor) ())

(defmethod execute ((this blocking-executor) (task function) &key (priority :medium))
  (put-into (task-queue-of this) task priority))

(definline make-blocking-executor (&optional queue-size)
  (make-instance 'blocking-executor :queue-size queue-size))


(defclass discarding-executor (generic-executor) ())

(defmethod execute ((this discarding-executor) (task function) &key
                                                                 (priority :medium)
                                                                 (important-p t)
                                                                 processing-thread)
  (if (or (not important-p) (and processing-thread
                                 (equal (current-thread) processing-thread)))
      (try-put-replacing (task-queue-of this) task priority)
      (put-into (task-queue-of this) task priority)))


(definline make-discarding-executor (&optional queue-size)
  (make-instance 'discarding-executor :queue-size queue-size))


;;;
;;;
;;;
(defclass single-threaded-executor (disposable)
  ((executor :initarg :executor :initform (error ":executor missing"))
   (processing-thread :initform nil)))


(defmethod initialize-instance :after ((this single-threaded-executor)
                                       &key special-variables)
  (with-slots (executor processing-thread) this
    (setf processing-thread
          (bt:make-thread (lambda ()
                            (progv special-variables
                                (mapcar (constantly nil) special-variables)
                              (ignite executor)))
                          :name "single-threaded-executor"))))


(define-destructor single-threaded-executor (executor)
  (dispose executor))


(definline make-single-threaded-executor (&key (queue-size +default-queue-size+)
                                               special-variables)
  "Make executor that run tasks in the same dedicated thread. Symbols specified in the
`special-variables` would be available to the tasks ran by this executor.

 Executor has maximum number of tasks it can take in until it block next call to #'execute if
:important-p key is not specified or set to 't. If :important-p is set to 'nil when maximum
number of tasks is reached new incoming tasks will be discarded until number of tasks will drop
below maximum allowed."
  (make-instance 'single-threaded-executor
                 :executor (make-discarding-executor queue-size)
                 :special-variables special-variables))


(defmethod alivep ((this single-threaded-executor))
  (with-slots (processing-thread) this
    (thread-alive-p processing-thread)))


(defmethod execute ((this single-threaded-executor) (task function) &key
                                                                      (priority :medium)
                                                                      (important-p t))
  "Execute task in the dedicated thread. Block execution of the #'execute caller if maximum
number of queued tasks is reached and :important-p is set to 't. Discard task if maximum number
of queued tasks is reached, but :important-p is set to nil or posting thread and processing
thread are the same."
  (with-slots (executor processing-thread) this
    (execute executor task :priority priority :important-p important-p
             :processing-thread processing-thread)))

;;;
;;;
;;;
(defclass pooled-executor (disposable)
  (dispatching-fu dispatching-instance))


(defmethod initialize-instance :after ((this pooled-executor) &key (size +default-pool-size+))
  (with-slots (dispatching-fu dispatching-instance) this
    (multiple-value-bind (function instance)
        (simple-flow-dispatcher:make-simple-dispatcher :threads size)
      (setf dispatching-fu function
            dispatching-instance instance))))


(define-destructor pooled-executor (dispatching-fu)
  (simple-flow-dispatcher:free-simple-dispatcher dispatching-fu))


(definline make-pooled-executor (&optional (size +default-pool-size+))
  "Make executor that run tasks concurrently in the dedicated thread-pool."
  (make-instance 'pooled-executor :size size))


(defmethod alivep ((this pooled-executor))
  (with-slots (dispatching-instance) this
    (simple-flow-dispatcher::simple-dispatcher-instance-alive-p dispatching-instance)))


(defmethod execute ((this pooled-executor) (task function) &key invariant (priority :medium))
  "Run task concurrently in the thread pool."
  (with-slots (dispatching-fu) this
    (funcall dispatching-fu task invariant :priority priority :ignore-invariant (null invariant))))
