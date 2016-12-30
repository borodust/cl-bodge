(in-package :cl-bodge.graphics)


(defclass graphics-system (thread-bound-system)
  ((host-sys :initform nil)
   (shader-loader :initform nil))
  (:default-initargs :depends-on '(host-system asset-system)))


(defmethod initialize-system :after ((this graphics-system))
  (with-slots (host-sys shader-loader) this
    (setf host-sys (host)
          shader-loader (make-instance 'shader-loader))
    (let ((reg (asset-registry-of (assets))))
      (register-asset-loader reg shader-loader))))


(defmethod discard-system :before ((this graphics-system))
  (clear-all-caches))


(defmethod make-system-context ((this graphics-system))
  (with-slots (host-sys) this
    (bind-rendering-context host-sys)
    (log:debug "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
               (gl:get* :version)
               (gl:get* :shading-language-version)
               (gl:get* :vendor)
               (gl:get* :renderer))
    (reset-state))
  nil)


(definline graphics ()
  (engine-system 'graphics-system))


(defmacro in-wireframe-mode (&body body)
  `(unwind-protect
        (progn
          (gl:polygon-mode :front-and-back :line)
          ,@body)
     (gl:polygon-mode :front-and-back :fill)))


(defmacro preserving-state (&body body)
  `(unwind-protect
        (progn ,@body)
     (reset-state)))
