(in-package :cl-bodge.distribution)


(defun list-foreign-dependencies (parent-library-path)
  (with-program-output (dep-string) ("otool -L \"~a\"" (namestring parent-library-path))
    (let ((deps (cddr (split-sequence:split-sequence #\Newline dep-string))))
      (loop for dep in deps
         for path = (trim-whitespaces (subseq dep 0 (position #\( dep)))
         when (> (length path) 0) collect path))))


(defun system-library-p (lib-pathname)
  (let ((path (namestring lib-pathname)))
    (or (starts-with-subseq "/System/Library/Frameworks" path)
        (ends-with-subseq "libobjc.A.dylib" path)
        (ends-with-subseq "libSystem.B.dylib" path))))


(defun list-platform-search-paths ()
  (list))
