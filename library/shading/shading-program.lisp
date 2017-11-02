(in-package :cl-bodge.library.shading)


(defun shading-program-source-name (program-name source-name)
  (engine-resource-name "shading-program/~(~A~)/~A"  program-name source-name))


(defclass shading-program-descriptor ()
  ((shader-sources :initform nil)
   (cached-program :initform nil)))


(defun clear-cached-program (program)
  (with-slots (cached-program) program
    (unless (null cached-program)
      (dispose cached-program))
    (setf cached-program nil)))


(defmethod initialize-instance :after ((this shading-program-descriptor) &key shader-sources)
  (with-slots ((this-shader-sources shader-sources)) this
    (flet ((load-shader-source (type-resource-pair)
             (destructuring-bind (type . name) type-resource-pair
               (make-shader-source name type (load-resource
                                              (shading-program-source-name (class-name-of this)
                                                                           name))))))
      (setf this-shader-sources (mapcar #'load-shader-source shader-sources))))
  (before-system-shutdown 'graphics-system (lambda () (clear-cached-program this))))


(defun shading-program-class-name (name)
  (format-symbol :cl-bodge.library.shading.program-descriptor name))


(defmacro define-shading-program (name-and-opts &body shader-sources)
  (let* ((opts (ensure-list name-and-opts))
         (name (first opts))
         (class-name (shading-program-class-name name))
         (descriptor-path (current-file-truename))
         (descriptor-directory (when descriptor-path (directory-namestring descriptor-path)))
         (shader-sources (loop for (type . (path . rest)) = shader-sources then rest
                            until (null type)
                            collecting (cons type path))))
    `(progn
       (defclass ,class-name (shading-program-descriptor) ()
         (:default-initargs
          :shader-sources ',shader-sources))
       ,@(loop for (type . path) in shader-sources
            collect `(mount-text-file (shading-program-source-name ',name ,path)
                                      ,(fad:merge-pathnames-as-file
                                        descriptor-directory path))))))


(defun load-shading-program (name)
  (make-instance (shading-program-class-name name)))


(define-system-function build-shading-program graphics-system (shading-program-descriptor)
  (with-slots (shader-sources) shading-program-descriptor
    (flet ((%compile-library (name-type)
             (destructuring-bind (name . type) name-type
               (let ((library-name (translate-name-from-foreign name)))
                 (compile-shader-library (load-shader-library library-name) type)))))
      (restart-case
          (loop with libs and processed-sources
             for source in shader-sources
             for type = (shader-type-of source)
             do (multiple-value-bind (text used-lib-names) (preprocess (shader-text-of source) type)
                  (nunionf libs (mapcar (lambda (name) (cons type name)) used-lib-names) :test #'equal)
                  (push (make-shader-source (shader-name-of source) type text)
                        processed-sources))
             finally
               (return (make-shading-program processed-sources
                                             :precompiled-shaders (mapcar #'%compile-library libs))))
        (reload-sources-and-build ()
          (build-shading-program (load-shading-program (class-of shading-program-descriptor))))))))
