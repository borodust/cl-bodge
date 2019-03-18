(bodge-util:define-package :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :bodge-util)
  (:reexport :bodge-util)
  (:export #:log/debug
           #:log/trace
           #:log/info
           #:log/warn
           #:log/error
           #:log/level))
(cl:in-package :cl-bodge.utils)


(defmacro log/trace (control-string &rest args)
  `(log:trace '(:cl-bodge) ,control-string ,@args))


(defmacro log/debug (control-string &rest args)
  `(log:debug '(:cl-bodge) ,control-string ,@args))


(defmacro log/info (control-string &rest args)
  `(log:info '(:cl-bodge) ,control-string ,@args))


(defmacro log/warn (control-string &rest args)
  `(log:warn '(:cl-bodge) ,control-string ,@args))


(defun log/level (&optional (level nil level-provided-p))
  (let ((logger (log:category '(:cl-bodge))))
    (if level-provided-p
        (log:config logger level)
        (log4cl:effective-log-level logger))))
