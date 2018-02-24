(cl:in-package :cl-bodge.library.shading)


(defenum library-shader-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader
  :compute-shader
  :any-shader)


(defun shader-resource-name (name file)
  (engine-resource-name "shader-library/~A/~A" name file))


(defun shader-header-resource-name (name)
  (shader-resource-name name "header"))


(defun shader-source-resource-name (name)
  (shader-resource-name name "source"))


(defclass shader-library ()
  ((name :initarg :name :type string :reader name-of)
   (types :initarg :shader-types :type library-shader-type :reader supported-shader-types)
   (header :reader header-of)
   (source :reader source-of)
   (shader-alist :initform nil)))


(defun clear-library-cache (library)
  (with-slots (shader-alist) library
    (loop for (type . shader) in shader-alist
       do (dispose shader)
       finally (setf shader-alist '()))))


(defmethod initialize-instance :after ((this shader-library) &key)
  (with-slots ((this-header header) (this-source source)) this
    (setf this-header (load-resource (shader-header-resource-name (name-of this)))
          this-source (load-resource (shader-source-resource-name (name-of this))))
    (before-system-shutdown 'graphics-system
                            (lambda () (clear-library-cache this)))))


(defun process-include (line)
  (let ((start (position #\< line))
        (end (position #\> line)))
    (when (or (null start) (null end))
      (error "Malformed include: \"~a\"" line))
    (let* ((lib-name (subseq line (1+ start) end))
           (lib (load-resource (shader-header-resource-name lib-name))))
      (when (null lib)
        (error "Library '~a' not found" lib-name))
      (values lib lib-name))))


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


(defun shader-library-class-name (name)
  (format-symbol :cl-bodge.library.shading.library-descriptor name))


(defmacro define-shader-library (name &key (types :any-shader)
                                        header source)
  (let ((stringified-name (translate-name-to-foreign name))
        (class-name (shader-library-class-name name))
        (path (current-file-truename)))
    `(progn
       (defclass ,class-name (shader-library) ()
         (:default-initargs :name ,stringified-name
           :shader-types ,types))
       ,@(when header
           `((defresource :text (shader-header-resource-name ,stringified-name)
               :path ,(fad:merge-pathnames-as-file path header))))
       ,@(when source
           `((defresource :text (shader-source-resource-name ,stringified-name)
               :path ,(fad:merge-pathnames-as-file path source)))))))


(defun load-shader-library (name)
  (make-instance (shader-library-class-name name)))

;;;
;;;
;;;
;; TODO unload shaders later
(define-system-function compile-shader-library graphics-system (name &optional shader-type)
  (let ((library (load-shader-library name)))
    (with-slots (shader-alist source) library
      (if-let ((shader (assoc-value shader-alist shader-type)))
        shader
        (let* ((types (ensure-list (supported-shader-types library)))
               (type (%select-shader-type (class-name-of library) shader-type types))
               (processed-source (preprocess source type))
               (source (make-shader-source (name-of library) type processed-source)))
          (setf (assoc-value shader-alist (shader-type-of source)) (compile-shader source)))))))
