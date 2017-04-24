(in-package :cl-bodge.distribution)


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
  (let* ((parent-path (fad:pathname-parent-directory path))
         (last-path-el (enough-namestring path parent-path))
         (name (or name (file last-path-el))))
    (inferior-shell:run/nil `("sh" "-c" ,(inferior-shell:token-string
					  `(,(format nil "cd \"~A\" && " parent-path)
					     ("zip -r " ,(format nil "~A.zip" name) " "
                                                        ,last-path-el)))))))


(defun find-dependencies-with-ldd (parent-library-path)
  (with-program-output (dep-string) ("ldd" (namestring parent-library-path))
    (let ((deps (split-sequence:split-sequence #\Newline dep-string)))
      (loop for dep in deps
         for div-idx = (search "=>" dep)
         for path-start-pos = (if div-idx (+ 2 div-idx) 0)
         for path-end-pos = (search "(0x" dep :from-end t)
         for path = (uiop:pathname-directory-pathname
                     (trim-whitespaces (subseq dep path-start-pos path-end-pos)))
         for lib = (if div-idx
                       (trim-whitespaces (subseq dep 0 div-idx))
                       (file-namestring path))
	 unless (equal lib "???") ; Windows crap 
         collect (cons lib path)))))
