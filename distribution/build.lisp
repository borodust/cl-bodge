(in-package :cl-bodge.distribution)


(declaim (special *distribution*))


(defun trim-whitespaces (string)
  (string-trim '(#\Space #\Tab #\Newline) string))


(labels ((%extract-name (sys-def)
           (if (listp sys-def)
               (ecase (first sys-def)
                 (:version (second sys-def)))
               sys-def))

         (%list-dependencies (system)
           (mapcar #'%extract-name (asdf:system-depends-on system)))

         (%proper-path-p (sys-path)
           (and sys-path
                (> (length (trim-whitespaces (namestring sys-path))) 0)))

         (%list-system-pathnames (system-designator)
           (let* ((system (asdf:find-system system-designator))
                  (sys-path (asdf:system-definition-pathname system)))
             (append (when (%proper-path-p sys-path) (list sys-path))
                     (loop for sys-name in (%list-dependencies system) append
                          (%list-system-pathnames sys-name))))))

  (defun list-system-pathnames (system-designator)
    (remove-duplicates (%list-system-pathnames system-designator) :test #'equal)))


(defun generate-manifest ()
  (let ((manifest-file (fad:merge-pathnames-as-file (build-directory-of *distribution*)
                                                    "manifest-file")))
    (alexandria:write-string-into-file (format nil "~{~a~^~%~}"
                                               (list-system-pathnames
                                                (target-system-of *distribution*)))
                                       manifest-file
                                       :if-exists :supersede)
    manifest-file))


(defun run-program (command-control-string &rest args)
  (uiop:run-program (apply #'format nil (nconc (list command-control-string) args))
                    :force-shell t :output *standard-output* :error-output *error-output*))


(defmacro with-program-output ((var) (control-string &rest args) &body body)
  `(let ((,var (with-output-to-string (stream)
                 (uiop:run-program (format nil ,control-string ,@args)
                                   :force-shell t
                                   :output stream
                                   :error-output *error-output*))))
     ,@body))


(defun build-executable ()
  (let ((manifest-file (generate-manifest)))
    (run-program "buildapp ~{\"~a\" ~}"
                 (list "--output" (fad:merge-pathnames-as-file (directory-of *distribution*)
                                                               (format nil "~(~a~).bin"
                                                                       (name-of *distribution*)))
                       "--entry" (entry-function-of *distribution*)
                       "--manifest-file" manifest-file
                       "--load-system" (format nil "~(~a~)" (target-system-of *distribution*))
                       #+sbcl "--compress-core"))))


(defun prepare ()
  (ensure-directories-exist (directory-of *distribution*)))


(defun copy-assets ()
  (labels ((copy-path (src dst)
             (flet ((walker (path)
                      (let ((last-el (enough-namestring path src)))
                        (if (fad:directory-pathname-p path)
                            (copy-path path (fad:merge-pathnames-as-directory dst last-el))
                            (copy-path path (fad:merge-pathnames-as-file dst last-el))))))
               (ensure-directories-exist dst)
               (if (fad:directory-pathname-p src)
                   (fad:walk-directory src #'walker :follow-symlinks nil)
                   (fad:copy-file src dst)))))
    (loop for (src dst) in (assets-of *distribution*) do
         (copy-path src dst))))


(defun compress ()
  (run-program "zip -r \"~a~(~a~).zip\" \"~a\""
               (build-directory-of *distribution*) (name-of *distribution*)
               (directory-of *distribution*)))


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


(defun copy-foreign-dependencies (lib-path lib-dir)
  (unless (system-library-p lib-path)
    (let ((dst (fad:merge-pathnames-as-file lib-dir (file-namestring lib-path))))
      (unless (fad:file-exists-p dst)
        (fad:copy-file lib-path dst)
        (loop for dep in (list-foreign-dependencies lib-path) do
             (copy-foreign-dependencies dep lib-dir))))))


(defun pack-foreign-libraries ()
  (let ((lib-dir (library-directory-of *distribution*))
        (dirs (cffi::parse-directories cffi:*foreign-library-directories*)))
    (ensure-directories-exist lib-dir)
    (loop for lib in (cffi:list-foreign-libraries) do
         (let ((path (cffi:foreign-library-pathname lib)))
           (if-let (file (cffi::find-file path dirs))
             (copy-foreign-dependencies file lib-dir)
             (error "Cannot find foreign library ~a" (cffi:foreign-library-name lib)))))))


(defun copy-engine-assets ()
  (let ((dir (engine-assets-directory-of *distribution*)))
    (when (asdf:system-registered-p :cl-bodge/assets)
      (when-let* ((pkg (find-package :ge.as))
                  (fsym (find-symbol (symbol-name 'copy-assets) pkg))
                  (copy-fn (symbol-function fsym)))
        (ensure-directories-exist dir)
        (funcall copy-fn dir)))))


(defun make-distribution (distribution-descriptor)
  (let* ((*distribution* (with-open-file (file distribution-descriptor)
                           (let ((*package* (find-package :ge.dist)))
                             (loop for form = (read file)
                                until (and (listp form) (eq (car form) 'define-distribution))
                                finally (return (eval (macroexpand form))))))))
    (load-system (target-system-of *distribution*) :verbose nil)
    (prepare)
    (build-executable)
    (pack-foreign-libraries)
    (copy-assets)
    (copy-engine-assets)
    (when (compressedp *distribution*)
      (compress))
    *distribution*))
