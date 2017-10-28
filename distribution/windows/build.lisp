(in-package :cl-bodge.distribution)

;;;
;;; For MSYS2 environment
;;;


(defun copy-runner ()
  (%copy-runner "windows/runner.template" "run.bat"))


(defun make-app-bundle ())
