(cl:in-package :cl-bodge.utils)


(defmacro in-development-mode (&body body)
  (declare (ignorable body))
  #-bodge-production-mode
  `(progn ,@body))


(defun invoke-bodgy (fu)
  (block skippable
    (handler-bind ((warning (lambda (w)
                              (log:warn w)
                              (return-from skippable)))
                   (t (lambda (e)
                        (dissect:with-capped-stack ()
                          (let ((error-text (with-output-to-string (stream)
                                              (format stream "Unhandled error:~%")
                                              (dissect:present e stream))))
                            (log:error "~a" error-text)
                            (in-development-mode
                              (break "~A: ~A" (type-of e) e)
                              (when-let ((continue-restart (find-restart 'continue)))
                                (invoke-restart continue-restart)))
                            (return-from skippable))))))
      (dissect:with-truncated-stack ()
        (funcall fu)))))


(defmacro log-errors (&body body)
  `(invoke-bodgy (lambda () ,@body)))


(defun epoch-seconds (&optional (timestamp (now)))
  (+ (timestamp-to-unix timestamp) (/ (nsec-of timestamp) 1000000000)))


(defun real-time-seconds ()
  (/ (get-internal-real-time) internal-time-units-per-second))


(defun current-file-truename ()
  (or *compile-file-truename* *load-truename* ""))


(defun current-executable-path ()
  (merge-pathnames (first (uiop:raw-command-line-arguments)) (uiop:getcwd)))
