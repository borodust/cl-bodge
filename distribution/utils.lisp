(in-package :cl-bodge.distribution)

(defun trim-whitespaces (string)
  (string-trim '(#\Space #\Tab #\Newline) string))


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


(defun temporary-directory (name &optional postfix)
  (loop for path = (fad:merge-pathnames-as-directory (uiop:temporary-directory)
                                                     (format nil "~a.~a"
                                                             name (or postfix
                                                                      (random (expt 2 64)))))
     while (fad:directory-exists-p path)
     finally
       (ensure-directories-exist path)
       (return path)))


(defmacro with-temporary-directory ((dir name &optional postfix) &body body)
  `(let ((,dir (temporary-directory ,name ,postfix)))
     (unwind-protect
          (progn ,@body)
       (fad:delete-directory-and-files ,dir))))


(defun path (&rest names)
  (apply #'fad:merge-pathnames-as-directory (mapcar #'fad:pathname-as-directory names)))


(defun file (&rest names)
  (let ((rev (nreverse names)))
    (apply #'fad:merge-pathnames-as-file
           (append (mapcar #'fad:pathname-as-directory (reverse (cdr rev)))
                   (list (fad:pathname-as-file (car rev)))))))


(defun copy-permissions (src dst)
  (declare (ignorable src dst))
  #+sbcl
  (let ((stat (sb-posix:stat src)))
    (sb-posix:chmod dst (sb-posix:stat-mode stat))))


(defun add-execution-permission (path)
  #+sbcl
  (let* ((stat (sb-posix:stat path))
         (mode (sb-posix:stat-mode stat)))
    (sb-posix:chmod path (logior mode sb-posix:s-iexec))))


(defun copy-path (src dst &optional observer)
  (flet ((walker (path)
           (let ((last-el (enough-namestring path src)))
             (if (fad:directory-pathname-p path)
                 (copy-path path (path dst last-el))
                 (copy-path path (file dst last-el))))))
    (ensure-directories-exist dst)
    (if (fad:directory-pathname-p src)
        (fad:walk-directory src #'walker :follow-symlinks nil)
        (progn
          (fad:copy-file src dst)
          (copy-permissions src dst)
          (when (functionp observer)
            (funcall observer src dst))))))


(defun compress-directory (path &optional name)
  (let* ((parent-path (fad:pathname-parent-directory path))
         (last-path-el (enough-namestring path parent-path))
         (name (or name (file last-path-el))))
  (run-program "cd \"~A\" && zip -r \"~A.zip\" \"~A\"" parent-path name last-path-el)))
