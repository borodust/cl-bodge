(cl:in-package :cl-bodge.distribution)


(define-constant +resource-filename+ "bodge.brf"
  :test #'equal)


(defgeneric configure-system (name)
  (:method (name) (declare (ignore name))))


(defun generate-manifest ()
  (let ((manifest-file (fad:merge-pathnames-as-file (build-directory-of *distribution*)
                                                    "manifest-file")))
    (ql:write-asdf-manifest-file manifest-file :if-exists :supersede)
    manifest-file))


(defun find-sbcl ()
  (or (when-let ((sbcl-home (uiop:getenv "SBCL_HOME")))
        (probe-file (file (dir sbcl-home) #p"../../bin/" *sbcl*)))
      *sbcl*))


(defun build-executable ()
  (labels ((%make-defvar (symbol value)
             (with-output-to-string (stream)
               (let ((*package* (find-package :cl)))
                 (prin1 `(defvar ,(format-symbol :cl-user symbol) ,value) stream))))
           (%defvar-asset-path (val)
             (%make-defvar '*bodge-asset-container-path* val)))
    (let ((output-file (file (directory-of *distribution*)
                             (wrap-executable-name
                              (executable-name-of *distribution*)))))
      (unless (fad:file-exists-p output-file)
        (let ((manifest-file (generate-manifest))
              (asset-file (enough-namestring
                           (file (asset-directory-of *distribution*) +resource-filename+)
                           (directory-of *distribution*))))
          (apply #'run-program (find-sbcl)
                 (append (list "--script" (file (distribution-system-path) "builder.lisp")
                               "--output" output-file
                               "--entry" (entry-function-of *distribution*)
                               "--manifest-file" manifest-file)
                         (loop for (binding . value) in (bindings-of *distribution*)
                            appending (list "--eval" (%make-defvar binding value)))
                         (list "--load" (file (distribution-system-path) "prologue.lisp")
                               "--eval" (%defvar-asset-path asset-file))
                         (when-let ((distribution-prologue (prologue-of *distribution*)))
                           (list "--load" distribution-prologue))
                         (list "--load-system" (format nil "~(~a~)" (target-system-of *distribution*))
                               "--load" (file (distribution-system-path) "epilogue.lisp"))
                         (when-let ((distribution-epilogue (epilogue-of *distribution*)))
                           (list "--load" distribution-epilogue)))))))))


(defun prepare ()
  (ensure-directories-exist (directory-of *distribution*)))


(defun copy-assets ()
  (loop for (src dst) in (resources-of *distribution*)
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


(defun serialize-assets-by-root (resource-root container-file)
  (unless (fad:file-exists-p container-file)
    (with-open-file (out container-file
                         :direction :output
                         :element-type '(unsigned-byte 8))
      (let ((flexi (flexi-streams:make-flexi-stream out :external-format :utf-8)))
        (prin1 '(:brf 1) flexi)
        (loop for resource-name in (ge.rsc:list-registered-resource-names)
           when (starts-with-subseq resource-root resource-name)
           do (let* ((asset (ge.rsc:load-resource resource-name))
                     (handler (ge.rsc:find-resource-handler resource-name))
                     (*package* (find-package :cl))
                     (*print-pretty* nil)
                     (data (flex:with-output-to-sequence (stream)
                             (ge.rsc:encode-resource handler asset stream))))
                (prin1 (list :encoded
                             :name resource-name
                             :size (length data))
                       flexi)
                (write-sequence data out)))))))


(defun serialize-assets ()
  (let* ((asset-dir (asset-directory-of *distribution*)))
    (ensure-directories-exist asset-dir)
    (loop for (resource-root container-path) in (cons (list "/_engine/" +resource-filename+)
                                                      (asset-containers-of *distribution*))
       do (serialize-assets-by-root resource-root (file asset-dir container-path)))))


(defun shout (string)
  (format t "~%~A~%" string)
  (finish-output))


(defun make-distribution (name &key build-directory)
  (let* ((*distribution* (distribution-by-name name))
         (*build-directory* build-directory))
    (let ((*load-verbose* nil)
          (*compile-verbose* nil)
          (*load-print* nil)
          (*compile-print* nil))
      (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (handler-bind ((style-warning #'muffle-warning))
        (load-system (target-system-of *distribution*) :verbose nil)))
    (shout "Running configuration")
    (configure-system name)
    (shout "Preparing build directory")
    (prepare)
    (shout "Building executable")
    (build-executable)
    (shout "Packing foreign libraries")
    (pack-foreign-libraries)
    (shout "Copying system assets")
    (copy-assets)
    (shout "Serializing registered assets")
    (serialize-assets)
    (when (compressedp *distribution*)
      (shout "Compressing distribution")
      (compress))
    (when (bundle-run-file-of *distribution*)
      (shout "Creating bundle")
      (make-app-bundle))
    (format t "~%Done~%")
    *distribution*))
