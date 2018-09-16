(cl:in-package :cl-bodge.graphics)


(declaim (special *shader-type*
                  *shader-dependencies*
                  *base-path*))


(defgeneric %header-of (shader)
  (:method (shader) (declare (ignore shader))))

(defgeneric %source-of (shader)
  (:method (shader) (declare (ignore shader))))

(defgeneric reload-shader-sources (shader))

(defgeneric %name-of (shader))

(defgeneric shader-descriptor-parameters (shader))

(defclass shader ()
  ((header :reader %header-of)
   (source :reader %source-of)
   (base-path :initarg :base-path :reader %base-path-of)
   (paths)
   (last-read-time :reader %last-read-time-of)))


(defun shader-changed-on-disk-p (shader)
  (with-slots (last-read-time paths) shader
    (loop for path in paths
            thereis (> (universal-time->epoch (cl:file-write-date path))
                       last-read-time))))


(defun %reload-shader-sources (shader header-paths source-paths)
  (with-slots (header source last-read-time paths base-path) shader
    (flet ((merge-base-path (pathname)
             (merge-pathnames pathname base-path)))
      (let ((full-header-paths (mapcar #'merge-base-path header-paths))
            (full-source-paths (mapcar #'merge-base-path source-paths)))
        (setf header (when full-header-paths
                       (format nil "~{~A~&~}" (mapcar #'read-file-into-string full-header-paths)))
              source (when full-source-paths
                       (format nil "~{~A~&~}" (mapcar #'read-file-into-string full-source-paths)))
              last-read-time (epoch-seconds)
              paths (append full-header-paths full-source-paths))))))


(defmethod initialize-instance :after ((this shader) &key)
  (reload-shader-sources this))


(defun expand-asdf-base-path (base-path)
  `(merge-pathnames
    (uiop:ensure-directory-pathname ,(or (second base-path) ""))
    (asdf:component-pathname (asdf:find-system ,(first base-path)))))


(defun expand-base-path (base-path)
  (let ((switch (first base-path)))
    (typecase switch
      (string switch)
      (symbol (if (eq :system-relative (first base-path))
                  (expand-asdf-base-path (rest base-path))
                  switch))
      (t switch))))


(defun default-library-name (name)
  (format nil "~A/~A"
          (translate-name-to-foreign (symbolicate (package-name (symbol-package name))))
          (translate-name-to-foreign name)))


(defmacro defshader (name-and-opts &body input)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key headers sources
                           ((:name stringified-name) (list (default-library-name name)))
                           (base-path (list (current-file-truename))))
        (alist-plist opts)
      (with-gensyms (this input-list)
        `(progn
           (defclass ,name (shader) ()
             (:default-initargs :base-path ,(expand-base-path base-path)))
           (defmethod %name-of ((,this ,name))
             ,@stringified-name)
           (let ((,input-list (list ,@(loop for parameter in input
                                            collect `(list ',(first parameter)
                                                           ,@(rest parameter))))))
             (defmethod shader-descriptor-parameters ((,this ,name))
               ,input-list))
           (defmethod reload-shader-sources ((,this ,name))
             (%reload-shader-sources ,this
                                     (list ,@headers)
                                     (list ,@sources)))
           (register-shader-library ',name)
           (make-instances-obsolete ',name))))))


(defun process-shader-type-name (type)
  (ecase type
    (:vertex-shader "BODGE_VERTEX_SHADER")
    (:tessellation-control-shader "BODGE_TESSELLATION_CONTROL_SHADER")
    (:tessellation-evaluation-shader "BODGE_TESSELLATION_EVALUATION_SHADER")
    (:geometry-shader "BODGE_GEOMETRY_SHADER")
    (:fragment-shader "BODGE_FRAGMENT_SHADER")
    (:compute-shader "BODGE_COMPUTE_SHADER")))


(defun process-version-directive (directive output)
  (format output "#~A~%#define ~A 1~%#define BODGE_SHADER 1"
          directive (process-shader-type-name *shader-type*)))


(defun %process-import-directive (lib-name output)
  (let* ((library (find-shader-library-by-name lib-name))
         (descriptor (shader-library-descriptor library))
         (header (%header-of descriptor)))
    (when (null header)
      (error "Header for library '~A' not found" lib-name))
    (pushnew (class-name-of descriptor) *shader-dependencies*)
    (preprocess-source header output)
    (format output "~%")))


(defun process-import-directive (directive output)
  (let ((start (position #\< directive))
        (end (position #\> directive)))
    (when (or (null start) (null end))
      (error "Malformed include directive: '#~A'" directive))
    (let ((lib-name (subseq directive (1+ start) end)))
      (%process-import-directive lib-name output))))


(defun process-include-directive (path output)
  (let ((start (position #\" path :from-end nil))
        (end (position #\" path  :from-end t)))
    (when (or (null start) (null end))
      (error "Malformed include path: '#~A'" path))
    (let ((pathname (subseq path (1+ start) end)))
      (format output "~%~A" (read-file-into-string (merge-pathnames pathname *base-path*))))))


(defun process-struct-use (commands output)
  (destructuring-bind (struct-type-name as qualifier type &optional block-name) commands
    (let ((struct-type (with-standard-io-syntax
                         (with-input-from-string (in struct-type-name)
                           (let ((*read-eval* nil))
                             (read in))))))
      (unless (equal "as" as)
        (error "Invalid use struct syntax: 'as' expected, but got ~A" as))
      (eswitch (type :test #'equal)
        ("block" (serialize-struct-as-interface struct-type qualifier block-name output))
        ("list" (serialize-struct-as-uniforms struct-type output))))))


(defun process-use-directive (commands output)
  (eswitch ((first commands) :test #'equal)
    ("struct" (process-struct-use (rest commands) output))))


(defun process-pragma-directive (directive output)
  (multiple-value-bind (pragma-start subdirective-start)
      (ppcre:scan "pragma\\s+bodge\\s*:\\s*" directive)
    (declare (ignore pragma-start))
    (if subdirective-start
        (let* ((subdirective-list (ppcre:split "\\s+" directive :start subdirective-start))
               (subdirective (first subdirective-list)))
          (unless subdirective
            (error "Malformed bodge pragma subdirective"))
          (eswitch (subdirective :test #'equal)
            ("import" (%process-import-directive (format nil "~{~A~}" (rest subdirective-list))
                                                 output))
            ("include" (process-include-directive (subseq directive (+ subdirective-start
                                                                       (length subdirective)))
                                                  output))
            ("use" (process-use-directive (rest subdirective-list) output))))
        (format output "~%#~A" directive))))


(defun process-directive (directive output)
  (switch (directive :test (lambda (directive prefix)
                             (starts-with-subseq prefix directive)))
    ("version" (process-version-directive directive output))
    ("include" (process-import-directive directive output))
    ("pragma" (process-pragma-directive directive output))
    (t (format output "~%#~A" directive))))


(defun preprocess-source (source output)
  (flet ((trim-string (string)
           (string-trim '(#\Space #\Tab) string)))
    (dolines (line source)
      (cond
        ((starts-with-subseq "#" (trim-string line))
         (process-directive (trim-string (subseq (trim-string line) 1)) output))
        (t (format output "~%~A" line))))))


(defun preprocess-shader (shader type)
  (let ((*shader-type* type)
        (*shader-dependencies* nil)
        (*base-path* (%base-path-of shader))
        (source (%source-of shader)))
    (values
     (with-output-to-string (output)
       (preprocess-source source output))
     (deletef *shader-dependencies* (class-name-of shader)))))
