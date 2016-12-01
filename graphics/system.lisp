(in-package :cl-bodge.graphics)


(defclass graphics-system (thread-bound-system)
  ((host-sys :initform nil))
  (:default-initargs :depends-on '(host-system)))


(defmethod initialize-system :after ((this graphics-system))
  (with-slots (host-sys) this
    (setf host-sys (engine-system 'host-system))))


(defmethod make-system-context ((this graphics-system))
  (with-slots (host-sys) this
    (bind-rendering-context host-sys)
    (log:info "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
              (gl:get* :version)
              (gl:get* :shading-language-version)
              (gl:get* :vendor)
              (gl:get* :renderer))
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:enable :blend
               :depth-test
               :program-point-size)
    (gl:blend-func :src-alpha :one-minus-src-alpha))
  nil)


(definline graphics ()
  (engine-system 'graphics-system))


(defmacro in-wireframe-mode (&body body)
  `(unwind-protect
        (progn
          (gl:polygon-mode :front-and-back :line)
          ,@body)
     (gl:polygon-mode :front-and-back :fill)))
