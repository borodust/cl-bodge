(in-package :cl-bodge.graphics)


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
