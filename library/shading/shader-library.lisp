(in-package :cl-bodge.library.shading)


(defenum library-shader-type
  :vertex-shader
  :tessellation-control-shader
  :tessellation-evaluation-shader
  :geometry-shader
  :fragment-shader
  :compute-shader
  :any-shader)


(defun shader-library-asset-name (name)
  (engine-external-resource-name "shader-library/descriptor/~(~A~)" name))


(defun shader-resource-name (name shader-type)
  (engine-resource-name "shader-library/~(~A~)/~(~A~)" shader-type name))


(defclass shader-library ()
  ((descriptor-path :initarg :descriptor-path)
   (name :initarg :name :type string :reader name-of)
   (types :initarg :shader-types :type library-shader-type :reader supported-shader-types)
   (descriptor-asset-name :initarg :descriptor-asset-name)
   (header :initarg :header :reader header-of)
   (source :initarg :source :reader source-of)
   (shader-alist :initform nil)))


(defun clear-library-cache (library)
  (with-slots (shader-alist) library
    (loop for (type . shader) in shader-alist
       do (dispose shader)
       finally (setf shader-alist '()))))


(defmethod initialize-instance :after ((this shader-library) &key)
  (before-system-shutdown 'graphics-system
                          (lambda () (clear-library-cache this))))


(defun process-include (line)
  (let ((start (position #\< line))
        (end (position #\> line)))
    (when (or (null start) (null end))
      (error "Malformed include: \"~a\"" line))
    (let* ((lib-name (subseq line (1+ start) end))
           (lib (get-resource (shader-library-asset-name lib-name))))
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


(defun load-source (path base)
  (read-file-into-string
   (if (fad:pathname-absolute-p path)
       (fad:canonical-pathname path)
       (fad:merge-pathnames-as-file base path))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %descriptor-path ()
    (or *compile-file-truename* *load-truename*)))


(defun decode-shader-library (class stream)
  (let ((flexi (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
    (destructuring-bind (header source) (read-preserving-whitespace flexi nil)
      (make-instance class :header header :source source))))


(defmacro define-shader-library (name &key ((:name glsl-name)) (types :any-shader)
                                        header source descriptor-path)
  (let ((class-name (symbolicate name))
        (path (or descriptor-path (%descriptor-path))))
    `(progn
       (defclass ,class-name (shader-library) ()
         (:default-initargs :descriptor-path ,path
           :name ,glsl-name
           :descriptor-asset-name (shader-library-asset-name ,glsl-name)
           :shader-types ,types
           :header (when ,header (load-source ,header ,path))
           :source (when ,source (load-source ,source ,path))))
       (defmethod decode-resource ((class (eql ',class-name)) stream)
         (decode-shader-library class stream))
       (in-development-mode
         (register-resource-loader (make-instance ',class-name))))))


;;;
;;;
;;;
;; TODO unload shaders later
(defun load-shader (library &optional shader-type)
  (with-slots (shader-alist source) library
    (if-let ((shader (assoc-value shader-alist shader-type)))
      shader
      (let* ((types (ensure-list (supported-shader-types library)))
             (type (%select-shader-type (class-name-of library) shader-type types))
             (processed-source (preprocess source type))
             (source (make-shader-source (name-of library) type processed-source)))
        (setf (assoc-value shader-alist (shader-type-of source)) (compile-shader source))))))


(defmethod list-resource-names ((this shader-library))
  (with-slots (descriptor-asset-name) this
    (list descriptor-asset-name)))


(defmethod load-resource ((this shader-library) name)
  (with-slots (descriptor-asset-name) this
    (when (equal name descriptor-asset-name)
      this)))


(defmethod encode-resource ((this shader-library) stream)
  (with-slots (header source) this
    (let ((flexi (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
      (prin1 (list header source) flexi))))
