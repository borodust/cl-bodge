(in-package :cl-bodge.assets)


(defun assets-root ()
  (when-let ((configured (property :engine-assets)))
    (merge-working-pathname (fad:pathname-as-directory configured))))


(defun make-engine-asset-id (prefix name-control-string &rest args)
  (with-output-to-string (name-stream)
    (format name-stream  "/engine/~(~A~)/" prefix)
    (apply #'format name-stream name-control-string args)))


(defmacro engine-asset-id (name-control-string &rest args)
  `(make-engine-asset-id ,(package-name *package*) ,name-control-string ,@args))
