(in-package :cl-bodge.utils)


(defmacro in-development-mode (&body body)
  (declare (ignorable body))
  #-bodge-production-mode
  `(progn ,@body))


(defmacro log-errors (&body body)
  (with-gensyms (name)
    `(block ,name
       (handler-bind ((warning (lambda (w)
                                 (log:warn w)
                                 (return-from ,name)))
                      (t (lambda (e)
			   (dissect:with-capped-stack ()
			     (let ((error-text (with-output-to-string (stream)
						 (format stream "Unhandled error:~%")
						 (dissect:present e stream))))
			       (log:error "~a" error-text)
			       (in-development-mode
				 (break "~A: ~A" (type-of e) e))
			       (return-from ,name))))))
         (dissect:with-truncated-stack () ,@body)))))


(defun epoch-seconds (&optional (timestamp (now)))
  (+ (timestamp-to-unix timestamp) (/ (nsec-of timestamp) 1000000000)))


(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))


(defmacro with-float-traps-masked (&body body)
  (let ((masking #+sbcl '(sb-int:with-float-traps-masked (:overflow
                                                          :underflow
                                                          :inexact
                                                          :invalid
                                                          :divide-by-zero))
                 #-sbcl '(progn)))
    `(,@masking
      ,@body)))



(defun current-file-truename ()
  (or *compile-file-truename* *load-truename* ""))


(defun current-executable-path ()
  (merge-pathnames (first (uiop:raw-command-line-arguments)) (uiop:getcwd)))
