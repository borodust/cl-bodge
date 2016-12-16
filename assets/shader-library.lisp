(in-package :cl-bodge.assets)


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
  ((descriptor-path :initarg :descriptor-path)
   (name :initarg :name :type string :reader name-of)
   (types :initarg :shader-types :type library-shader-type)
   (header-path :initarg :header-path)
   (source-path :initarg :source-path)
   (shader-alist :initform nil)))


(defmethod path-to ((this shader-library))
  (with-slots (descriptor-path) this
    (fad:canonical-pathname
     (fad:pathname-directory-pathname
      (if (fad:pathname-relative-p descriptor-path)
          (fad:merge-pathnames-as-file (assets-root) descriptor-path)
          descriptor-path)))))


(defmethod assets-of ((this shader-library))
  (with-slots (header-path source-path descriptor-path) this
    (let ((descriptor-dir (fad:pathname-directory-pathname descriptor-path))
          (assets (list descriptor-path)))
      (when header-path
        (push (fad:merge-pathnames-as-file descriptor-dir header-path) assets))
      (when source-path
        (push (fad:merge-pathnames-as-file descriptor-dir source-path) assets))
      assets)))


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


(defun process-shader-type-name (type)
  (ecase type
    (:vertex-shader "VERTEX_SHADER")
    (:tessellation-control-shader "TESSELLATION_CONTROL_SHADER")
    (:tessellation-evaluation-shader "TESSELLATION_EVALUATION_SHADER")
    (:geometry-shader "GEOMETRY_SHADER")
    (:fragment-shader "FRAGMENT_SHADER")
    (:compute-shader "COMPUTE_SHADER")))


(defun preprocess (raw-source &optional type)
  (loop with libs and source = raw-source and type-inserted-p
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
                   ((and (not type-inserted-p)
                         (starts-with-subseq "#version"
                                             (string-left-trim '(#\Space #\Tab) line))
                         type
                         (not (eq type :any-shader)))
                    (format processed "~a~%#define ~a 1" line (process-shader-type-name type))
                    (setf preprocessed-p t
                          type-inserted-p t))
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


(defgeneric header-of (library))
(defgeneric shader-source (library &optional type))

(flet ((load-source (path base &optional type)
         (preprocess (read-file-into-string
                      (if (fad:pathname-absolute-p path)
                          (fad:canonical-pathname path)
                          (fad:merge-pathnames-as-file base path)))
                     type)))

  (defmethod header-of((this shader-library))
    (with-slots (header-path) this
      (if header-path
          (load-source header-path (path-to this))
          (list ""))))


  (defmethod shader-source ((this shader-library) &optional type)
      (with-slots (source-path types) this
        (unless (null source-path)
          (let* ((shader-types (if (listp types) types (list types)))
                 (type (%select-shader-type (class-name-of this) type shader-types)))
            (make-shader-source (file-namestring (fad:canonical-pathname source-path))
                                type
                                (load-source source-path (path-to this) type)))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %descriptor-path ()
    (or *compile-file-truename* *load-truename*)))


(defmacro define-shader-library (name &key ((:name glsl-name)) (types :any-shader)
                                        header source descriptor-path)
  `(progn
     (defclass ,(symbolicate name) (shader-library) ()
       (:default-initargs :descriptor-path ',(or descriptor-path (%descriptor-path))
         :name ,glsl-name
         :shader-types ,types
         :header-path ,header
         :source-path ,source))
     (register-library ',name)))


;;;
;;;
;;;
;; TODO unload shaders later
(defun load-shader (library &optional shader-type)
  (with-slots (name types shader-alist) library
    (when-let ((source (shader-source library shader-type)))
      (let ((shader (assoc (cons name (shader-type-of source)) shader-alist :test #'equal)))
        (if (null shader)
            (cdar (push (cons (cons name (shader-type-of source))
                              (compile-shader source))
                        shader-alist))
            (cdr shader))))))


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


(defun clear-library-cache (library)
  (with-slots (shader-alist) library
    (loop for (type . shader) in shader-alist
       do (dispose shader)
       finally (setf shader-alist '()))))


(defun clear-all-library-caches ()
  (loop for lib being the hash-value in (sm-libs *shader-manager*)
     do (clear-library-cache lib)))



(defun build-shading-program (shader-sources)
  (restart-case
      (loop with libs and processed-sources
         for source in shader-sources
         for type = (shader-type-of source)
         do
           (multiple-value-bind (text used-lib-names) (preprocess (shader-text-of source) type)
             (loop for name in used-lib-names
                for shader = (load-shader (library-by-name name) type)
                unless (null shader) do (pushnew shader libs))
             (push (make-shader-source (shader-name-of source) type text)
                   processed-sources))
         finally
           (return (build-separable-shading-program processed-sources libs)))
    (reload-sources-and-build ()
      (build-shading-program (loop for source in shader-sources collecting
                                  (reload-shader-text source))))))
