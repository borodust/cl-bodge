(in-package :cl-bodge.assets)


(defgeneric path-to (obj))

(defgeneric assets-of (obj))


(defun assets-root ()
  (if-let ((configured (property :engine-assets)))
    (merge-working-pathname (fad:pathname-as-directory configured))
    (asdf:component-pathname (asdf:find-system :cl-bodge/assets))))
