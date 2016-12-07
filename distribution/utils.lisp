(in-package :cl-bodge.distribution)

(defun trim-whitespaces (string)
  (string-trim '(#\Space #\Tab #\Newline) string))


(defun run-program (command-control-string &rest args)
  (uiop:run-program (apply #'format nil (nconc (list command-control-string) args))
                    :force-shell t :output *standard-output* :error-output *error-output*))


(defmacro with-program-output ((var) (control-string &rest args) &body body)
  `(let ((,var (with-output-to-string (stream)
                 (uiop:run-program (format nil ,control-string ,@args)
                                   :force-shell t
                                   :output stream
                                   :error-output *error-output*))))
     ,@body))
