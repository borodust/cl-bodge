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
  (with-guarded-reference (queue (job-queue-queue job-queue))
    (loop until (null queue) for job = (pop queue)
       do (funcall job))))
