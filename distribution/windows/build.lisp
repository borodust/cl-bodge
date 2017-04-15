(in-package :cl-bodge.distribution)

;;;
;;; For MSYS2 environment
;;;


(defun convert-msys-namestring (pathname)
  (let ((path (namestring pathname)))
    (format nil "~A:~A" (aref path 1) (subseq path 2))))


(define-constant +system-libraries+
    (list "System32")
  :test #'equal)


(defun list-foreign-dependencies (parent-library-path)
  (find-dependencies-with-ldd parent-library-path))


(defun system-library-p (lib-pathname)
  (let ((path (namestring lib-pathname)))
    (flet ((substringp (substring)
             (search substring path :test #'equalp)))
      (or (some #'substringp +system-libraries+)))))


(defun list-platform-search-paths ()
  (append (list "c:/Windows/System32/")
	  (split-sequence:split-sequence #\; (uiop:getenv "PATH"))))


(defun make-app-bundle ())
