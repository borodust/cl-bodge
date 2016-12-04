(in-package :cl-bodge.assets)


(defun copy-assets (destination-path)
  (let ((destination (fad:pathname-as-directory destination-path))
        (source (assets-root)))
    (unless (fad:directory-exists-p destination)
      (error "destination directory not found: ~a" destination))
    (let ((assets (loop for lib being the hash-value in (sm-libs *shader-manager*) append
                       (assets-of lib))))
      (dolist (asset-path assets)
        (let ((asset-destination (fad:merge-pathnames-as-file destination asset-path)))
          (ensure-directories-exist (fad:pathname-directory-pathname asset-destination))
          (fad:copy-file (fad:merge-pathnames-as-file source asset-path)
                         asset-destination))))))
