(in-package :cl-bodge.graphics)


(defstruct rendering-context
  (scene nil)
  (job-queue (make-job-queue) :read-only t))

(defclass graphics-system (system)
  ((enabled-p :initform nil)
   (rendering-context :initform (make-rendering-context) :reader rendering-context-of)
   (state-lock :initform (make-lock "gx-sys-state-lock"))
   (state-condi-var :initform (make-condition-variable
                               :name "gx-sys-state-condi-var"))
   (app-sys :initform nil))
  (:default-initargs :dependencies '(application-system)))


(defmethod enable ((this graphics-system))
  (with-slots (enabled-p app-sys state-lock state-condi-var) this
    (with-lock-held (state-lock)
      (when enabled-p
        (error "Graphics system already enabled"))
      (bt:make-thread
       (lambda ()
         (log-errors
           (setf app-sys (engine-system 'application-system))
           (bind-rendering-context app-sys)
           (log:info "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
                     (gl:get* :version)
                     (gl:get* :shading-language-version)
                     (gl:get* :vendor)
                     (gl:get* :renderer))
           (with-lock-held (state-lock)
             (setf enabled-p t))
           (condition-notify state-condi-var)
           (log:info "Rendering loop running")
           (let ((*rendering-context* (rendering-context-of this)))
             (declare (special *rendering-context*))
             (loop while enabled-p for scene = (rendering-context-scene *rendering-context*)
                unless (null scene)
                do (log-errors (render scene))
                do (log-errors
                     (drain (rendering-context-job-queue *rendering-context*))
                     (swap-buffers app-sys)
                     (sleep 0.01)))
             (condition-notify state-condi-var))
           (log:info "Rendering loop stopped. Graphics system offline")))
       :name "rendering-worker")
      (loop until enabled-p do
           (condition-wait state-condi-var state-lock)))))


(defmethod disable ((this graphics-system))
  (with-slots (enabled-p state-lock state-condi-var rendering-context) this
    (with-lock-held (state-lock)
      (unless enabled-p
        (error "Graphics system already enabled"))
      (push-body-into ((rendering-context-job-queue rendering-context))
        (with-slots (enabled-p) this
          (with-lock-held (state-lock)
            (setf enabled-p nil))))
      (loop while enabled-p do
           (condition-wait state-condi-var state-lock)))))


(defun render-scene (renderable rendering-context)
  (push-body-into ((rendering-context-job-queue rendering-context))
    (setf (rendering-context-scene rendering-context) renderable)))

(defmacro within-rendering-context ((&optional (ctx-var (gensym "ctx"))
                                               (gx-sys-var (gensym "gx-sys"))) gx-sys
                                    &body body)
  (once-only (gx-sys)
    `(push-body-into ((rendering-context-job-queue (rendering-context-of ,gx-sys)))
       (declare (special *rendering-context*))
       (let ((,ctx-var *rendering-context*)
             (,gx-sys-var ,gx-sys))
         (declare (ignorable ,ctx-var ,gx-sys-var))
         ,@body))))
         
