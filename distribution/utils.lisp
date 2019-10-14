(cl:in-package :cl-bodge.distribution)


(declaim (special *distribution*))

(defun wrap-executable-name (name)
  (format nil "~A~A" name #+windows ".exe" #-windows ""))

(defun zip (relative-directory archive-name &optional (executable "zip"))
  (list (wrap-executable-name executable) "-v" "-r" archive-name relative-directory))


(defvar *lisp* (first (uiop:raw-command-line-arguments)))
(defvar *zip* #'zip)


(defun distribution-system-path ()
  (asdf:component-pathname (find-system :cl-bodge/distribution)))


(defun trim-whitespaces (string)
  (string-trim '(#\Space #\Tab #\Newline) string))


(defun launch-program (output command)
  (inferior-shell:run command :output output :error-output *error-output*))


(defun run-program (command &rest args)
  (funcall #'launch-program *standard-output* (append (list command) args)))


(defmacro with-program-output ((var) (command &rest args) &body body)
  (with-gensyms (stream)
    `(let ((,var (with-output-to-string (,stream)
		   (launch-program ,stream (list ,command ,@args)))))
       ,@body)))


(defun generate-random-name ()
  (format nil "~:@(~36,8,'0R~)" (random (expt 36 8))))


(defun temporary-directory (name &optional postfix)
  (loop for path = (fad:merge-pathnames-as-directory
                    (uiop:temporary-directory)
                    (format nil "~A.~A" name (or postfix (generate-random-name))))
     while (fad:directory-exists-p path)
     finally
       (ensure-directories-exist path)
       (return path)))


(defmacro with-temporary-directory ((dir name &optional postfix) &body body)
  `(let ((,dir (temporary-directory ,name ,postfix)))
     (unwind-protect
          (progn ,@body)
       (fad:delete-directory-and-files ,dir))))


(defun dir (&rest names)
  (apply #'fad:merge-pathnames-as-directory (mapcar #'fad:pathname-as-directory names)))


(defun file (&rest names)
  (let ((rev (reverse names)))
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


(defun copy-path (source destination &optional observer)
  (labels ((%copy-path (src dst)
             (ensure-directories-exist dst)
             (fad:copy-file src dst)
             (copy-permissions src dst)
             (when (functionp observer)
               (funcall observer src dst)))
           (walker (path)
             (let ((last-el (enough-namestring path source)))
               (%copy-path path (file destination last-el)))))
    (if (fad:directory-pathname-p source)
        (fad:walk-directory source #'walker :follow-symlinks nil)
        (%copy-path source destination))))


(defun compress-directory (path &optional name)
  (let* ((parent-path (fad:canonical-pathname (fad:pathname-parent-directory path)))
         (last-path-el (enough-namestring (fad:canonical-pathname path) parent-path))
         (name (or name (file last-path-el))))
    (multiple-value-bind (output error-output code)
        (uiop:with-current-directory ((namestring parent-path))
          (inferior-shell:run/nil (if (functionp *zip*)
                                      (funcall *zip* last-path-el
                                               (inferior-shell:token-string
                                                (list name ".zip")))
                                      (zip last-path-el
                                           (inferior-shell:token-string
                                            (list name ".zip"))
                                           *zip*))
                                  :output :string
                                  :error-output :string
                                  :show t
                                  :on-error nil))
      (unless (= code 0)
        (error "~A~&~A" output error-output)))))
