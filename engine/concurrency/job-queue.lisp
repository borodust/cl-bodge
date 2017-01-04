(in-package :cl-bodge.concurrency)

;; fifo
(defstruct task-queue
  (queue (make-guarded-reference '()) :read-only t))


(defun push-task (task-fu task-queue)
  (with-guarded-reference (queue (task-queue-queue task-queue))
    (nconcf queue (list task-fu))))


(defmacro push-body-into ((task-queue) &body body)
  `(push-task (lambda () ,@body) ,task-queue))


(defun drain (task-queue &optional invoker)
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
  (with-guarded-reference (queue (task-queue-queue task-queue))
    (setf queue '())))
