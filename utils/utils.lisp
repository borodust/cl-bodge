(cl:in-package :cl-bodge.utils)


(defmacro in-development-mode (&body body)
  (declare (ignorable body))
  #-bodge-production-mode
  `(progn ,@body))


(defun invoke-continue-restart ()
  (when-let ((continue-restart (find-restart 'continue)))
    (invoke-restart continue-restart)))


(defun invoke-bodgy (fu)
  (block skippable
    (macrolet ((with-error-report-string ((report) c &body body)
                 (once-only (c)
                   `(dissect:with-capped-stack ()
                      (let ((,report (with-output-to-string (stream)
                                       (format stream "Unhandled condition:~%")
                                       (dissect:present ,c stream))))
                        ,@body)))))
      (handler-bind ((serious-condition (lambda (e)
                                          (with-error-report-string (error-text) e
                                            (log:error "~A" error-text)
                                            (in-development-mode
                                              (break "~A: ~A" (type-of e) e)
                                              (invoke-continue-restart))
                                            (return-from skippable))))
                     (t (lambda (e)
                          (with-error-report-string (error-text) e
                            (log:warn "~A" error-text)))))
        (dissect:with-truncated-stack ()
          (funcall fu))))))


(defmacro log-errors (&body body)
  `(invoke-bodgy (lambda () ,@body)))


(defun current-file-truename ()
  (or *compile-file-truename* *load-truename* ""))


(defun current-executable-path ()
  (merge-pathnames (first (uiop:raw-command-line-arguments)) (uiop:getcwd)))


(defun system-relative-pathname (system-name pathname)
  (merge-pathnames pathname (asdf:component-pathname (asdf:find-system system-name))))


(defun shout (control-string &rest args)
  (format t "~&~A" (apply #'format nil control-string args))
  (finish-output t))
