(in-package :cl-bodge.graphics)


(defstruct (rendering-context
             (:conc-name rc-))
  (scene nil))

(defclass graphics-system (thread-bound-system)
  ((host-sys :initform nil))
  (:default-initargs :dependencies '(host-system)))


(defmethod initialize-system :after ((this graphics-system))
  (with-slots (host-sys) this
    (setf host-sys (engine-system 'host-system))
    (bind-rendering-context host-sys)
    (log:info "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
              (gl:get* :version)
              (gl:get* :shading-language-version)
              (gl:get* :vendor)
              (gl:get* :renderer))))


(defmethod make-system-context ((this graphics-system))
  (make-rendering-context))


(defmethod execute-looping-action :after ((this graphics-system))
  (with-slots (host-sys) this
    (when-let ((scene (rc-scene *system-context*)))
      (render scene))
    (swap-buffers host-sys)
    (sleep 0.01)))


(defun render-scene (renderable rendering-context)
  (setf (rc-scene rendering-context) renderable))
