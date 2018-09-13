(cl:in-package :cl-bodge.graphics)


(defstruct (graphics-context
             (:conc-name ctx-))
  (state (make-instance 'context-state) :read-only t)
  (supplementary-framebuffer nil)
  (framebuffer-width 0 :type fixnum)
  (framebuffer-height 0 :type fixnum)
  (depth-stencil-renderbuffer nil))


(defclass graphics-system (thread-bound-system)
  (resource-executor)
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


(defmacro for-graphics ((&optional arg) &body body)
  `(ge.ng:-> (graphics) (,@(when arg (list arg)))
     ,@body))


(defmacro for-shared-graphics ((&optional arg) &body body)
  `(ge.ng:-> (graphics) :main-p nil (,@(when arg (list arg)))
     ,@body))


(defmethod enabling-flow ((this graphics-system))
  (with-slots (resource-executor) this
    (ge.ng:>> (call-next-method)
        (for-host ()
          (execute resource-executor
                   (lambda ()
                     (bind-rendering-context :main nil)
                     (glad:init)
                     (log:debug "Shared context bound"))
                   :priority :highest :important-p t)
          (framebuffer-size))
        (ge.ng:-> this (viewport)
          (declare (type vec2 viewport))
          (update-context-framebuffer-size (floor (x viewport))
                                           (floor (y viewport)))))))



(defmethod dispatch ((this graphics-system) (task function) invariant &rest args
                     &key (main-p t) disposing)
  (with-slots (resource-executor) this
    (flet ((run-task ()
             (let ((*system* this)
                   (*system-context* (system-context-of this))
                   (*supplementary-framebuffer*
                     (ctx-supplementary-framebuffer (system-context-of this)))
                   (*depth-stencil-renderbuffer*
                     (ctx-depth-stencil-renderbuffer (system-context-of this))))
               (funcall task))))
      (if main-p
          (if disposing
              (apply #'call-next-method this #'run-task invariant
                     :important-p t :priority :highest args)
              (apply #'call-next-method this #'run-task invariant args))
          (execute resource-executor #'run-task)))))


(define-system-function reset-viewport graphics-system ()
  (gl:viewport 0 0
               (ctx-framebuffer-width *system-context*)
               (ctx-framebuffer-height *system-context*)))


(define-system-function viewport-width graphics-system ()
  (ctx-framebuffer-width *system-context*))


(define-system-function viewport-height graphics-system ()
  (ctx-framebuffer-height *system-context*))


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
      (unless (featurep :bodge-gl2)
        (gx.state:enable :texture-cube-map-seamless)
        (setf (ctx-supplementary-framebuffer ctx) (gl:gen-framebuffer)
              (ctx-depth-stencil-renderbuffer ctx) (gl:gen-renderbuffer)))
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
      (gx.state:pixel-store :unpack-alignment 1)
      ctx)))


(defmethod destroy-system-context ((this graphics-system) ctx)
  (gl:delete-framebuffers (list (ctx-supplementary-framebuffer ctx)))
  (gl:delete-renderbuffers (list (ctx-depth-stencil-renderbuffer ctx)))
  (clear-registry-cache)
  (release-rendering-context))


(definline graphics ()
  (engine-system 'graphics-system))


(defun update-context-framebuffer-size (width height)
  (setf (ctx-framebuffer-width *system-context*) width
        (ctx-framebuffer-height *system-context*) height))


(define-event-handler on-framebuffer-size-change ((ev framebuffer-size-change-event) width height)
  (run (for-graphics () (update-context-framebuffer-size width height))))


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
