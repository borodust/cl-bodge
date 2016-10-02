(in-package :cl-bodge.utils)

(defmacro log-errors (&body body)
  `(handler-case
       (progn ,@body)
     (t (e) (log:error "Unhandled error: ~a" e))))


(defun read-file-into-string-list (pathname)
  (split-sequence:split-sequence "#\Newline" (read-file-into-string pathname)))
