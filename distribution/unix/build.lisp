(in-package :cl-bodge.distribution)


(defun copy-runner ()
  (add-execution-permission (%copy-runner "unix/runner.template" "run.sh")))


(defun make-app-bundle ())
