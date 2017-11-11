(cl:require :asdf)
(cl:require :uiop)

(cl:defpackage :cl-bodge.builder
  (:use :cl)
  (:export build-executable))
(cl:in-package :cl-bodge.builder)


(defun shout (control &rest args)
  (format t "~A~%" (apply #'format nil control args))
  (finish-output))


(defun load-manifest (pathname)
  (let ((manifest (uiop:split-string (uiop:read-file-string pathname) :separator '(#\Newline)))
        (system-table (make-hash-table :test #'equal)))
    (dolist (path manifest)
      (setf (gethash (pathname-name path) system-table) (probe-file path)))
    (flet ((%find-system (name)
             (gethash name system-table)))
      (pushnew #'%find-system asdf:*system-definition-search-functions*))))


(defun build-executable ()
  (let ((args (uiop:command-line-arguments))
        (working-dir (merge-pathnames (directory-namestring (first (uiop:raw-command-line-arguments)))
                                      (uiop:getcwd)))
        (init-sequence)
        (entry-function)
        (output-file)
        (manifest))
    (flet ((%load (pathname)
             (lambda ()
               (shout "Loading file '~A'" pathname)
               (uiop:load* (merge-pathnames pathname working-dir))))
           (%eval (value)
             (lambda ()
               (shout "Evaluating '~A'" value)
               (uiop:eval-input value)))
           (%load-system (system-name)
             (lambda ()
               (shout "Loading system '~A'" system-name)
               (asdf:load-system system-name :verbose nil))))
      (loop with loaders
         for (option argument) on args by #'cddr
         do (cond
              ((equal option "--load") (push (%load (merge-pathnames argument working-dir))
                                                    loaders))
              ((equal option "--eval") (push (%eval argument) loaders))
              ((equal option "--load-system") (push (%load-system argument) loaders))
              ((equal option "--manifest-file") (setf manifest argument))
              ((equal option "--entry") (setf entry-function argument))
              ((equal option "--output") (setf output-file argument))
              (t (error "Unexpected option '~A' option" option)))
         finally (setf init-sequence (nreverse loaders))))
    (unless output-file
      (error "--output must be specified" ))
    (unless entry-function
      (error "--entry must be specified" ))
    (when manifest
      (let ((manifest-path (merge-pathnames manifest working-dir)))
        (shout "Loading manifest '~A'" manifest-path)
        (load-manifest manifest-path)))
    (dolist (loader init-sequence)
      (funcall loader))
    (setf uiop:*image-entry-point* (uiop:ensure-function entry-function))
    (shout "Using entry function '~A'" entry-function)
    (ensure-directories-exist output-file)
    (shout "Dumping executable to '~A'" output-file)
    (apply #'uiop:dump-image (merge-pathnames output-file working-dir)
           :executable t
           (append #+(and sbcl sb-core-compression) (list :compression 9)
                   #+(and sbcl windows) (list :application-type :gui)))))

(build-executable)
