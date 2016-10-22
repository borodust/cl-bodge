(in-package :cl-bodge.resources)


(defenum library-shader-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader
  :compute-shader
  :any-shader)


(defstruct (shader-manager
             (:conc-name sm-))
  (libs (make-hash-table :test #'equal) :read-only t))

(defvar *shader-manager* (make-shader-manager))


(defclass shader-library ()
  ((descriptor-path :initarg :descriptor-path :reader path-to)
   (name :initarg :name :type string :reader name-of)
   (types :initarg :shader-types :type library-shader-type)
   (header-path :initarg :header-path)
   (source-path :initarg :source-path)
   (uniforms :initarg :uniforms :reader uniforms-of)
   (shader-alist :initform nil)))



(flet ((load-source (path base)
         (read-file-into-string
          (if (fad:pathname-absolute-p path)
              (fad:canonical-pathname path)
              (fad:merge-pathnames-as-file base path)))))

  (defgeneric header-of (library)
    (:method ((this shader-library))
      (with-slots (header-path) this
        (if header-path
            (load-source header-path (path-to this))
            (list "")))))


  (defgeneric source-of (library)
    (:method ((this shader-library))
      (with-slots (source-path) this
        (load-source source-path (path-to this))))))


(defmacro define-shader-library (name &key ((:name glsl-name)) (types :any-shader)
                                        header source uniforms)
  `(progn
     (defclass ,(symbolicate name) (shader-library) ()
       (:default-initargs :descriptor-path ,(or *compile-file-truename* *load-truename*)
         :name ,glsl-name
         :shader-types ,types
         :header-path ,header
         :source-path ,source
         :uniforms ',uniforms))
     (register-library ',name)))


;;;
;;;
;;;
(defun process-include (line)
  (let ((start (position #\< line))
        (end (position #\> line)))
    (when (or (null start) (null end))
      (error "Malformed include: \"~a\"" line))
    (let* ((lib-name (subseq line (1+ start) end))
           (lib (library-by-name lib-name)))
      (when (null lib)
        (error "Library '~a' not found" lib-name))
      (values (header-of lib) lib-name))))


(defun preprocess (raw-source)
  (loop with libs and source = raw-source
     for preprocessed-p = nil do
       (setf source
             (with-output-to-string (processed)
               (dolines (line source)
                 (cond
                   ((starts-with-subseq "#include"
                                        (string-left-trim '(#\Space #\Tab) line))
                    (multiple-value-bind (source lib-name) (process-include line)
                      (unless (member lib-name libs :test #'equal)
                        (unless preprocessed-p
                          (setf preprocessed-p t))
                        (push lib-name libs)
                        (format processed "~%~a" source))))
                   (t (format processed "~%~a" line))))))
     while preprocessed-p
     finally (return (values source libs))))


(defun %select-shader-type (lib-name shader-type shader-types)
  (if (null shader-type)
      (if (or (rest shader-types) (member :any-shader shader-types))
          (error "Library ~a supports ~a shader types. Please, specify one in `shader-type`."
                 lib-name shader-types)
          (first shader-types))
      (if (or (member shader-type shader-types)
              (member :any-shader shader-types))
          shader-type
          (error "Library ~a doesn't support compilation for ~a" lib-name shader-type))))


;; TODO unload shaders later
(defun load-shader (gx-sys library &optional shader-type)
  (with-slots (name types shader-alist) library
    (let* ((shader-types (if (listp types) types (list types)))
           (type (%select-shader-type (class-name-of library)
                                      shader-type shader-types))
           (shader (assoc (cons name type) shader-alist :test #'equal)))
      (if (null shader)
          (cdar (push (cons (cons name type)
                            (compile-shader gx-sys type
                                            (preprocess (source-of library))))
                      shader-alist))
          (cdr shader)))))


;;;
;;;
;;;
(defun register-library (classname)
  (let ((lib (make-instance classname)))
    (with-hash-entries ((lib-entry (name-of lib))) (sm-libs *shader-manager*)
      (unless (null lib-entry)
        (warn "Library with name '~a' was registered earlier" (name-of lib)))
      (setf lib-entry lib))))


(defun library-by-name (name)
  (gethash name (sm-libs *shader-manager*)))


(defun build-shading-program (gx-sys &rest shader-sources)
  (loop with libs and processed-sources
     for source in shader-sources
     for type = (shader-type-of source)
     do
       (multiple-value-bind (source used-lib-names) (preprocess (shader-text-of source))
         (loop for name in used-lib-names do
              (pushnew (load-shader gx-sys (library-by-name name) type) libs))
         (push (make-instance 'shader-source :text source :type type) processed-sources))
     finally
       (return (build-separable-shading-program gx-sys processed-sources libs))))
