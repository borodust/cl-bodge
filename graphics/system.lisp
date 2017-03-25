(in-package :cl-bodge.graphics)


(defstruct (graphics-context
             (:conc-name ctx-))
  (state (make-instance 'context-state) :read-only t))


(defclass graphics-system (thread-bound-system)
  ((host-sys :initform nil)
   (state :initform nil)
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
    (glad:init)

    (let ((ctx (make-graphics-context)))
      (with-current-state-slice ((ctx-state ctx))
        (gx.state:enable :blend
                         :cull-face
                         :depth-test
                         :program-point-size)
        (gx.state:disable :scissor-test
                          :stencil-test)

        (gx.state:cull-face :back)
        (gx.state:front-face :ccw)
        (gx.state:clear-color 1.0 1.0 1.0 1.0)
        (gx.state:color-mask t t t t)
        (gx.state:clear-depth 1.0)
        (gx.state:blend-func :src-alpha :one-minus-src-alpha)
        (gx.state:blend-func-separate :src-alpha :one-minus-src-alpha
                                      :one :zero)
        (gx.state:clear-stencil 0)
        (gx.state:stencil-mask #xffffffff)
        (gx.state:stencil-func :always 0 #xffffffff)
        (gx.state:stencil-op :keep :keep :keep)

        ctx))))


(definline graphics ()
  (engine-system 'graphics-system))


(define-system-function reset-state graphics-system ()
  (reset-context-state (ctx-state *system-context*)))


(defmacro preserving-state (&body body)
  `(with-state-slice ((preserve-state (ctx-state *system-context*)))
     (unwind-protect
          (progn ,@body)
       (restore-state (ctx-state *system-context*)))))


(defmacro in-wireframe-mode (&body body)
  `(unwind-protect
        (progn
          (gl:polygon-mode :front-and-back :line)
          ,@body)
     (gl:polygon-mode :front-and-back :fill)))
