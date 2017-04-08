(in-package :cl-bodge.distribution)

;;;
;;; For MSYS2 environment
;;;


(define-constant +system-libraries+
    (list "System32")
  :test #'equal)


(defun list-foreign-dependencies (parent-library-path)
  (with-program-output (dep-string) ("ldd \"~a\"" (namestring parent-library-path))
    (let ((deps (split-sequence:split-sequence #\Newline dep-string)))
      (loop for dep in deps
         for div-idx = (search "=>" dep)
         for path = (trim-whitespaces (subseq dep
                                              (if div-idx (+ 2 div-idx) 0)
                                              (position #\( dep)))
         when (> (length path) 0) collect path))))


(defun system-library-p (lib-pathname)
  (let ((path (namestring lib-pathname)))
    (flet ((substringp (substring)
             (search substring path :test #'equalp)))
      (or (some #'substringp +system-libraries+)))))



(defun list-platform-search-paths ()
  (list "c:/Windows/System32/"))


(defun make-app-bundle ())
