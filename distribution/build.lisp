(in-package :cl-bodge.distribution)


(define-constant +engine-resource-filename+ "bodge.brf"
  :test #'equal)


(defun generate-manifest ()
  (let ((manifest-file (fad:merge-pathnames-as-file (build-directory-of *distribution*)
                                                    "manifest-file")))
    (ql:write-asdf-manifest-file manifest-file :if-exists :supersede)
    manifest-file))


(defun build-executable ()
  (let ((output-file (fad:merge-pathnames-as-file (directory-of *distribution*)
                                                  (format nil "~(~a~).bin"
                                                          (name-of *distribution*)))))
    (unless (fad:file-exists-p output-file)
      (let ((manifest-file (generate-manifest))
            (asset-file (fad:merge-pathnames-as-file (engine-assets-directory-of *distribution*)
                                                     +engine-resource-filename+)))
        (unless (fad:file-exists-p output-file)
	  (run-program *buildapp*
		       "--output" output-file
		       "--entry" (entry-function-of *distribution*)
		       "--manifest-file" manifest-file
                       "--load-system" "bodge-blobs"
		       "--load" (file (distribution-system-path) "prologue.lisp")
		       "--load-system" (format nil "~(~a~)" (target-system-of *distribution*))
		       "--eval" (format nil "(defvar *engine-assets-path* \"~A\")"
					asset-file)
		       "--load" (file (distribution-system-path) "epilogue.lisp")
		       #-windows ;; SBCL on windows does not support compression
		       "--compress-core"))))))


(defun prepare ()
  (ensure-directories-exist (directory-of *distribution*)))


(defun copy-assets ()
  (loop for (src dst) in (assets-of *distribution*)
     unless (fad:file-exists-p dst)
     do (copy-path src dst)))


(defun compress ()
  (compress-directory (directory-of *distribution*)
                      (format nil "~(~A~)" (name-of *distribution*))))


(defun pack-foreign-libraries ()
  (let ((lib-dir (library-directory-of *distribution*)))
    (ensure-directories-exist lib-dir)
    (loop for lib-path in (bodge-blobs-support:list-registered-libraries)
       do (let* ((name (file-namestring lib-path))
                 (target-file (fad:merge-pathnames-as-file lib-dir name)))
            (fad:copy-file lib-path target-file)))))


(defun serialize-engine-assets ()
  (let* ((engine-asset-dir (path (directory-of *distribution*)
                             (engine-assets-directory-of *distribution*)))
         (engine-asset-file (file engine-asset-dir
                                  +engine-resource-filename+)))
    (ensure-directories-exist engine-asset-dir)
    (unless (fad:file-exists-p engine-asset-file)
      (with-open-file (out engine-asset-file
                           :direction :output
                           :element-type '(unsigned-byte 8))
        (let ((flexi (flexi-streams:make-flexi-stream out :external-format :utf-8)))
          (prin1 '(:brf 1) flexi)
          (dolist (resource-name (ge.rsc:list-registered-resource-names))
            (let ((asset (ge.rsc:load-resource resource-name))
                  (*package* (find-package :cl)))
              (prin1 (list :encoded
                           :asset-class (ge.util:class-name-of asset)
                           :name resource-name)
                     flexi)
              (ge.rsc:encode-resource (ge.rsc:find-resource-handler resource-name) asset out))))))))


(defun shout (string)
  (format t "~%~A~%" string)
  (finish-output))


(defun make-distribution (name)
  (let* ((*distribution* (distribution-by-name name)))
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil))
      (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (handler-bind ((style-warning #'muffle-warning))
        (load-system (target-system-of *distribution*) :verbose nil)))
    (shout "Preparing build directory")
    (prepare)
    (shout "Building executable")
    (build-executable)
    (copy-runner)
    (shout "Packing foreign libraries")
    (pack-foreign-libraries)
    (shout "Copying system assets")
    (copy-assets)
    (shout "Copying engine assets")
    (serialize-engine-assets)
    (when (compressedp *distribution*)
      (shout "Compressing distribution")
      (compress))
    (when (bundle-run-file-of *distribution*)
      (shout "Creating bundle")
      (make-app-bundle))
    (format t "~%Done~%")
    *distribution*))
