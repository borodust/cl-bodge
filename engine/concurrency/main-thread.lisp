(cl:in-package :cl-bodge.concurrency)

(defmacro with-body-in-main-thread (() &body body)
  `(,@ #+darwin '(trivial-main-thread:with-body-in-main-thread ()) #-darwin' (in-new-thread "main-thread")
                ,@body))


(defun stop-main-runner ()
  #+darwin (trivial-main-thread:stop-main-runner))
