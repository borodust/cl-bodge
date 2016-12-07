(in-package :cl-bodge.distribution)

(define-constant +system-path+ (asdf:component-pathname (find-system :cl-bodge/distribution))
  :test #'equal)


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


(defun make-bundle-runner (bundle-name run-file target-dir)
  (ensure-directories-exist target-dir)
  (let ((runner-template (read-file-into-string (file +system-path+ "macos-runner.template")))
        (runner-file (file target-dir bundle-name)))
    (write-string-into-file (format nil runner-template run-file) runner-file)
    (add-execution-permission runner-file)))


(defun make-app-bundle ()
  (let* ((bundle-name (bundle-name-of *distribution*))
         (dist-dir (directory-of *distribution*))
         (bundle-root (path (build-directory-of *distribution*)
                            (format nil "~A.app/" bundle-name)))
         (content-dir (path bundle-root "Contents/MacOS")))
    (copy-path dist-dir bundle-root)
    (make-bundle-runner bundle-name (bundle-run-file-of *distribution*) content-dir)
    (add-execution-permission bundle-root)
    (when (bundle-compressed-p *distribution*)
      (compress-directory bundle-root (format nil "~A-macos" bundle-name)))))
