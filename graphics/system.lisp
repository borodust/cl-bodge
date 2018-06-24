(cl:in-package :cl-bodge.graphics)


(defstruct (graphics-context
             (:conc-name ctx-))
  (state (make-instance 'context-state) :read-only t))


(defclass graphics-system (thread-bound-system)
  ((resource-executor)
   (state :initform nil))
  (:default-initargs :depends-on '(host-system)))


(defmethod initialize-system :after ((this graphics-system))
  (with-slots (resource-executor) this
    (setf resource-executor (acquire-executor :single-threaded-p t :exclusive-p t))))


(defmethod discard-system :before ((this graphics-system))
  (with-slots (resource-executor) this
    (when (alivep resource-executor)
      (mt:wait-with-latch (latch)
        (execute resource-executor
                 (lambda ()
                   (unwind-protect
                        (progn
                          (release-rendering-context)
                          (log:debug "Shared context released"))
                     (mt:open-latch latch)))
                 :priority :highest :important-p t)))
    (release-executor resource-executor)))


(defmethod enabling-flow ((this graphics-system))
  (with-slots (resource-executor) this
    (>> (call-next-method)
        (for-host ()
          (execute resource-executor
                   (lambda ()
                     (bind-rendering-context :main nil)
                     (glad:init)
                     (log:debug "Shared context bound"))
                   :priority :highest :important-p t)))))


(defmethod dispatch ((this graphics-system) (task function) invariant &key (main-p t))
  (with-slots (resource-executor) this
    (flet ((run-task ()
             (let ((*system* this)
                   (*system-context* (system-context-of this)))
               (funcall task))))
      (if main-p
          (call-next-method)
          (execute resource-executor #'run-task)))))


(defmethod make-system-context ((this graphics-system))
  (bind-rendering-context)
  (log:debug "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
             (gl:get* :version)
             (gl:get* :shading-language-version)
             (gl:get* :vendor)
             (gl:get* :renderer))
  (glad:init)
  (log:debug "GLAD initialized")
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

      ctx)))


(defmethod destroy-system-context (ctx (this graphics-system))
  (clear-registry-cache)
  (release-rendering-context))


(definline graphics ()
  (engine-system 'graphics-system))


(defmacro for-graphics ((&optional arg) &body body)
  `(-> (graphics) (,@(when arg (list arg)))
     ,@body))


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
