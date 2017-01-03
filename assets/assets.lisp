(in-package :cl-bodge.assets)


(defun assets-root ()
  (when-let ((configured (property :engine-assets)))
    (merge-working-pathname (fad:pathname-as-directory configured))))


(defun engine-asset-id (name-control-string &rest args)
  (with-output-to-string (name-stream)
    (format name-stream  "/engine/")
    (apply #'format name-stream name-control-string args)))
