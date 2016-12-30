(in-package :cl-bodge.graphics)


(defstruct (shading-program-registry
             (:conc-name spr-))
  (program-table (make-hash-table :test #'eq) :read-only t))


(defvar *shading-program-manager* (make-shading-program-registry))


(defun list-shading-program-descriptors ()
  (loop for prog being the hash-value of (spr-program-table *shading-program-manager*)
     collecting prog))


(defun register-shading-program-descriptor (program-class)
  (with-hash-entries ((program program-class))
      (spr-program-table *shading-program-manager*)
    (if (null program)
        (setf program (make-instance program-class))
        (warn "Attempt to register already registered shading program of class ~a"
              program-class))))


(defclass shading-program-descriptor ()
  ((descriptor-path :initarg :descriptor-path)
   (shader-sources :initarg :shader-sources)
   (cached-program :initform nil)))


(defmacro define-shading-program (name-and-opts &body shader-sources)
  (let* ((opts (ensure-list name-and-opts))
         (name (first opts))
         (descriptor-path (%descriptor-path))
         (descriptor-directory (directory-namestring descriptor-path)))
    `(progn
       (defclass ,(symbolicate name) (shading-program-descriptor) ()
         (:default-initargs :descriptor-path ',descriptor-path
           :shader-sources (list ,@(loop for (type . (path . rest)) = shader-sources
                                      then rest until (null type)
                                      for full-path = (fad:merge-pathnames-as-file
                                                       descriptor-directory path)
                                      collecting `(load-shader-source ,type ,full-path)))))
       (register-shading-program-descriptor ',name))))


(define-system-function build-shading-program graphics-system (shader-sources)
  (restart-case
      (loop with libs and processed-sources
         for source in shader-sources
         for type = (shader-type-of source)
         do (multiple-value-bind (text used-lib-names) (preprocess (shader-text-of source) type)
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


(defun find-program-descriptor (program-class)
  (let ((program (gethash program-class (spr-program-table *shading-program-manager*))))
    (when (null program)
      (error "Cannot find shading program of class ~a" program-class))
    program))


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


(defun clear-all-caches ()
  (clear-all-library-caches)
  (loop for prog being the hash-value in (spr-program-table *shading-program-manager*)
     do (clear-cached-program prog)))
