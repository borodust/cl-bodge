(cl:in-package :cl-bodge.graphics)


(declaim (special *shader-type*
                  *shader-dependencies*))


(defgeneric %header-of (shader)
  (:method (shader) (declare (ignore shader))))

(defgeneric %source-of (shader)
  (:method (shader) (declare (ignore shader))))

(defgeneric reload-shader-sources (shader))

(defgeneric %name-of (shader))

(defgeneric shader-descriptor-parameters (shader))

(defclass shader ()
  ((last-updated :initform 0 :reader shader-descriptor-last-updated)
   (header :reader %header-of)
   (source :reader %source-of)))


(defun %reload-shader-sources (shader header-path source-path)
  (with-slots (header source) shader
    (setf header (when header-path (read-file-into-string header-path))
          source (when source-path (read-file-into-string source-path)))))


(defmethod initialize-instance :after ((this shader) &key)
  (reload-shader-sources this))


(defmethod update-instance-for-redefined-class :after ((this shader)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (last-updated) this
    (setf last-updated (real-time-seconds))
    (reload-shader-sources this)))


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


(defmacro defshader (name-and-opts &body input)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key header source
                           ((:name stringified-name) (list (translate-name-to-foreign name)))
                           (base-path (list (current-file-truename))))
        (alist-plist opts)
      (with-gensyms (%base-path this input-list)
        `(progn
           (defclass ,name (shader) ())
           (defmethod %name-of ((,this ,name))
             ,@stringified-name)
           (let ((,input-list (list ,@(loop for parameter in input
                                            collect `(list ',(first parameter)
                                                           ,@(rest parameter))))))
             (defmethod shader-descriptor-parameters ((,this ,name))
               ,input-list))
           (defmethod reload-shader-sources ((,this ,name))
             (let ((,%base-path (uiop:ensure-directory-pathname ,(expand-base-path base-path))))

               (%reload-shader-sources ,this
                                       ,(when header `(merge-pathnames ,@header ,%base-path))
                                       ,(when source `(merge-pathnames ,@source ,%base-path)))))
           (register-shader-library ',name)
           (make-instances-obsolete ',name))))))


(defun process-shader-type-name (type)
  (ecase type
    (:vertex-shader "VERTEX_SHADER")
    (:tessellation-control-shader "TESSELLATION_CONTROL_SHADER")
    (:tessellation-evaluation-shader "TESSELLATION_EVALUATION_SHADER")
    (:geometry-shader "GEOMETRY_SHADER")
    (:fragment-shader "FRAGMENT_SHADER")
    (:compute-shader "COMPUTE_SHADER")))


(defun process-version-directive (directive output)
  (format output "#~A~%#define ~A 1" directive (process-shader-type-name *shader-type*)))


(defun process-include-directive (directive output)
  (let ((start (position #\< directive))
        (end (position #\> directive)))
    (when (or (null start) (null end))
      (error "Malformed include: '#~A'" directive))
    (let* ((lib-name (subseq directive (1+ start) end))
           (library (find-shader-library-by-name lib-name))
           (descriptor (shader-library-descriptor library))
           (header (%header-of descriptor)))
      (when (null header)
        (error "Header for library '~A' not found" lib-name))
      (pushnew (class-name-of descriptor) *shader-dependencies*)
      (preprocess-source header output)
      (format output "~%"))))


(defun process-directive (directive output)
  (switch (directive :test (lambda (directive prefix)
                             (starts-with-subseq prefix directive)))
    ("version" (process-version-directive directive output))
    ("include" (process-include-directive directive output))
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
        (source (%source-of shader)))
    (values
     (with-output-to-string (output)
       (preprocess-source source output))
      *shader-dependencies*)))
