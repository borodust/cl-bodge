(in-package :cl-bodge.concurrency)

;; fifo
(defstruct job-queue
  (queue (make-guarded-reference '()) :read-only t))


(defun push-job (job-fu job-queue)
  (with-guarded-reference (queue (job-queue-queue job-queue))
    (nconcf queue (list job-fu))))


(defmacro push-body-into ((job-queue) &body body)
  `(push-job (lambda () ,@body) ,job-queue))


(defun drain (job-queue)
  (loop for count = 0 then (1+ count)
     for job = (with-guarded-reference (queue (job-queue-queue job-queue))
                 (pop queue))
     until (null job) do (funcall job)
     finally (return count)))


(defun clearup (job-queue)
  (with-guarded-reference (queue (job-queue-queue job-queue))
    (setf queue '())))
