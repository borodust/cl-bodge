(in-package :cl-bodge.distribution)


(defun list-foreign-dependencies (parent-library-path)
  (error "Don't know how to list foreign dependencies for this platform"))


(defun system-library-p (lib-pathname)
  (error "Don't know how to determine system library for this platform"))


(defun list-platform-search-paths ()
  (list))
