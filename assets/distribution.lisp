(in-package :cl-bodge.assets)


(defun copy-assets (destination-path)
  (let* ((destination (fad:pathname-as-directory destination-path))
         (shader-path (fad:merge-pathnames-as-directory destination-path "shaders/")))
    (unless (fad:directory-exists-p destination)
      (error "destination directory not found: ~a" destination))
    (let ((assets (loop for lib being the hash-value in (sm-libs *shader-manager*) append
                       (assets-of lib))))
      (dolist (asset-path assets)
        (let ((asset-destination (fad:merge-pathnames-as-file shader-path
                                                              (file-namestring asset-path))))
          (ensure-directories-exist (fad:pathname-directory-pathname asset-destination))
          (when (fad:file-exists-p asset-destination)
            (error "Asset exists: ~a" asset-destination))
          (fad:copy-file asset-path asset-destination))))))
