(in-package :cl-bodge.library.shading)


(defun shading-program-descriptor-asset-name (program-name)
  (engine-external-resource-name "shading-program/descriptor/~(~A~)"  program-name))

(defun shading-program-resource-name (program-name)
  (engine-resource-name "shading-program/~(~A~)"  program-name))


(defclass shading-program-descriptor ()
  ((descriptor-path :initarg :descriptor-path :initform nil)
   (descriptor-resource-name :initarg :descriptor-resource-name)
   (program-resource-name :initarg :program-resource-name)
   (shader-sources :initarg :shader-sources :initform nil)
   (cached-program :initform nil)))


(defun encode-shader-sources (sources)
  (loop for source in sources
     collect (list (list :type (shader-type-of source)
                         :name (shader-name-of source))
                   (shader-text-of source))))


(defmethod encode-resource ((this shading-program-descriptor) stream)
  (with-slots (shader-sources) this
    (let ((flexi (flexi-streams:make-flexi-stream stream :external-format :utf-8)))
      (prin1 (encode-shader-sources shader-sources) flexi))))


(defun decode-shader-sources (stream)
  (let* ((flexi (flexi-streams:make-flexi-stream stream :external-format :utf-8))
         (sources (read-preserving-whitespace flexi nil)))
    (loop for source in sources
       collecting (destructuring-bind ((&key name type) text) source
                    (make-shader-source name type text)))))


(defmacro define-shading-program (name-and-opts &body shader-sources)
  (let* ((opts (ensure-list name-and-opts))
         (name (first opts))
         (class-name (symbolicate name))
         (descriptor-path (%descriptor-path))
         (descriptor-directory (when descriptor-path (directory-namestring descriptor-path))))
    `(progn
       (defclass ,class-name (shading-program-descriptor) ()
         (:default-initargs :descriptor-path ',descriptor-path
           :descriptor-resource-name ,(shading-program-descriptor-asset-name class-name)
           :program-resource-name ,(shading-program-resource-name name)
           :shader-sources (list ,@(loop for (type . (path . rest)) = shader-sources
                                      then rest until (null type)
                                      for full-path = (fad:merge-pathnames-as-file
                                                       descriptor-directory path)
                                      collecting `(load-shader-source ,type ,full-path)))))
       (defmethod decode-resource ((this (eql ',class-name)) stream)
         (make-instance this
                        :descriptor-path nil
                        :shader-sources (decode-shader-sources stream)))
       (in-development-mode
         (register-resource-loader (make-instance ',class-name))))))


(define-system-function build-shading-program graphics-system (shader-sources)
  (restart-case
      (loop with libs and processed-sources
         for source in shader-sources
         for type = (shader-type-of source)
         do (multiple-value-bind (text used-lib-names) (preprocess (shader-text-of source) type)
              (loop for name in used-lib-names
                 for shader = (load-shader (get-resource (shader-library-asset-name name)) type)
                 unless (null shader) do (pushnew shader libs))
              (push (make-shader-source (shader-name-of source) type text)
                    processed-sources))
         finally
           (return (build-separable-shading-program processed-sources libs)))
    (reload-sources-and-build ()
      (build-shading-program (loop for source in shader-sources collecting
                                  (reload-shader-text source))))))


(define-system-function load-shading-program graphics-system (program-descriptor)
  (with-slots (cached-program shader-sources) program-descriptor
    (if (null cached-program)
        (setf cached-program (build-shading-program shader-sources))
        cached-program)))


(defun clear-cached-program (program)
  (with-slots (cached-program) program
    (unless (null cached-program)
      (dispose cached-program))
    (setf cached-program nil)))


;;;
;;;
;;;
(defmethod list-resource-names ((this shading-program-descriptor))
  (with-slots (program-resource-name descriptor-resource-name) this
    (list program-resource-name descriptor-resource-name)))


(defmethod load-resource ((this shading-program-descriptor) name)
  (with-slots (descriptor-resource-name program-resource-name) this
    (eswitch (name :test #'equal)
      (descriptor-resource-name (values this t))
      (program-resource-name (-> ((graphics)) ()
                               (load-shading-program this))))))


(defmethod release-resource ((this shading-program-descriptor) name)
  (with-slots (descriptor-resource-name program-resource-name) this
    (when (equal name program-resource-name)
      (-> ((graphics)) ()
        (clear-cached-program this)))))
