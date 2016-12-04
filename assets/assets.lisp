(in-package :cl-bodge.assets)


(defgeneric path-to (obj))

(defgeneric assets-of (obj))


(defun assets-root ()
  (fad:pathname-as-directory (property :assets-root
                                       (asdf:component-pathname
                                        (asdf:find-system :cl-bodge/assets)))))
