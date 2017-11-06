(in-package :cl-bodge.distribution)


(defun make-bundle-runner (bundle-name run-file target-dir)
  (ensure-directories-exist target-dir)
  (let ((runner-template (read-file-into-string (file (distribution-system-path)
                                                      "darwin/"
                                                      "macos-runner.template")))
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
