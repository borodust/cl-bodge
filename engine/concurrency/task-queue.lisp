(cl:in-package :cl-bodge.concurrency)

;; lifo
(defstruct (task-queue
             (:constructor %make-task-queue))
  (queue (make-guarded-reference '()) :read-only t))


(definline make-task-queue ()
  "Make trivial thread-safe task LIFO queue."
  (%make-task-queue))


(defun push-task (task-fu task-queue)
  "Push task function to the end of the queue. Thread-safe."
  (with-guarded-reference (queue (task-queue-queue task-queue))
    (nconcf queue (list task-fu))))


(defmacro push-body-into ((task-queue) &body body)
  "Push block of code to execute as a task to the end of the queue. Thread-safe."
  `(push-task (lambda () ,@body) ,task-queue))


(defun drain (task-queue &optional invoker)
  "Execute tasks in LIFO order once. If `invoker` function provided, invoke it with task
function as an argument instead. Thread-safe."
  (loop for count = 0 then (1+ count)
     do (multiple-value-bind (queue-empty-p task)
            (with-guarded-reference (queue (task-queue-queue task-queue))
              (values (null queue) (pop queue)))
          (if queue-empty-p
              (return count)
              (if invoker
                  (funcall invoker task)
                  (funcall task))))))


(defun clearup (task-queue)
  "Remove all tasks from the queue. Thread-safe."
  (with-guarded-reference (queue (task-queue-queue task-queue))
    (setf queue '())))
