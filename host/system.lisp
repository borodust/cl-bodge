(cl:in-package :cl-bodge.host)

(declaim (special *window*))

(defclass host-system (dispatching generic-system)
  ((enabled-p :initform nil)
   (swap-interval :initform 0)
   (shared :initform nil)
   (window :initform nil :reader window-of)
   (task-queue :initform (make-task-queue))))


(definline host ()
  (engine-system 'host-system))


(defmacro for-host ((&optional arg) &body body)
  `(-> (host) (,@(when arg (list arg)))
     ,@body))


(defmethod enabledp ((this host-system))
  (slot-value this 'enabled-p))


(defmethod dispatch ((this host-system) (fn function) invariant &key)
  (with-slots (task-queue) this
    (with-system-lock-held (this)
      (push-task fn task-queue)
      (%glfw:post-empty-event)))
  t)


(glfw:define-window-close-callback on-close (window)
  (%glfw:hide-window window)
  (post 'viewport-hiding-event))


(glfw:define-key-callback on-key-action (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (post 'keyboard-event
        :key (glfw-enumval->keyboard-key key)
        :state (glfw-enumval->button-state action)))


(glfw:define-mouse-button-callback on-mouse-action (window button action mod-keys)
  (declare (ignore window mod-keys))
  (post 'mouse-event
              :button (glfw-enumval->mouse-button button)
              :state (glfw-enumval->button-state action)))


(glfw:define-cursor-pos-callback on-cursor-movement (window x y)
  (claw:c-with ((width :int)
               (height :int))
    (%glfw:get-window-size window (width &) (height &))
    (post 'cursor-event :x x :y (- height y))))


(glfw:define-scroll-callback on-scroll (window x y)
  (declare (ignore window))
  (post 'scroll-event :x-offset x :y-offset (- y)))


(glfw:define-framebuffer-size-callback on-framebuffer-size-change (window w h)
  (declare (ignore window))
  (post 'framebuffer-size-change-event :width w :height h))


(glfw:define-framebuffer-size-callback on-viewport-size-change (window w h)
  (declare (ignore window))
  (post 'viewport-size-change-event :width w :height h))


(glfw:define-char-callback on-character-input (window char-code)
  (declare (ignore window))
  (let ((character (code-char char-code)))
    (post 'character-input-event :character character)))


(defun create-window (width height title gl-major-version gl-minor-version
                      &key (shared (cffi:null-pointer)) (visible nil))
  (glfw:with-window-hints ((%glfw:+context-version-major+ gl-major-version)
                           (%glfw:+context-version-minor+ gl-minor-version)
                           (%glfw:+opengl-profile+ %glfw:+opengl-core-profile+)
                           (%glfw:+opengl-forward-compat+ %glfw:+true+)
                           (%glfw:+depth-bits+ 24)
                           (%glfw:+stencil-bits+ 8)
                           (%glfw:+resizable+ %glfw:+false+)
                           (%glfw:+visible+ (if visible %glfw:+true+ %glfw:+false+)))
    (%glfw:create-window width height title (cffi:null-pointer) shared)))


(defun make-shared-context (gl-major-version gl-minor-version)
  (prog1 (create-window 1 1 "" gl-major-version gl-minor-version :shared *window*)
    (%glfw:make-context-current (cffi:null-pointer))))


(defun init-callbacks ()
  (%glfw:set-window-close-callback *window* (claw:callback 'on-close))
  (%glfw:set-key-callback *window* (claw:callback 'on-key-action))
  (%glfw:set-mouse-button-callback *window* (claw:callback 'on-mouse-action))
  (%glfw:set-cursor-pos-callback *window* (claw:callback 'on-cursor-movement))
  (%glfw:set-scroll-callback *window* (claw:callback 'on-scroll))
  (%glfw:set-framebuffer-size-callback *window* (claw:callback 'on-framebuffer-size-change))
  (%glfw:set-window-size-callback *window* (claw:callback 'on-viewport-size-change))
  (%glfw:set-char-callback *window* (claw:callback 'on-character-input)))


;; if current thread is the main one, this function will block
(defmethod initialize-system :after ((this host-system))
  (with-slots (enabled-p task-queue window eve-sys gl-major-version gl-minor-version shared) this
    (when enabled-p
      (error "Host system already enabled"))
    (wait-with-latch (latch)
      (log:debug "Injecting loop into main thread")
      (with-body-in-main-thread ()
        (unwind-protect
             (log-errors
               (destructuring-bind (major-version minor-version)
                   (property '(:host :opengl-version) '(4 1))
                 (log:debug "Initializing GLFW context for OpenGL version ~A.~A"
                            major-version minor-version)
                 (claw:with-float-traps-masked ()
                   (glfw:with-init ()
                     (let ((*window* (create-window 640 480 "Scene" major-version minor-version
                                                    :visible t)))
                       (unwind-protect
                            (progn
                              (init-callbacks)
                              (%glfw:swap-interval 0)
                              (%glfw:make-context-current (cffi:null-pointer))
                              (log:debug "GL context detached from main loop thread")
                              (setf window *window*
                                    shared (make-shared-context major-version minor-version)
                                    enabled-p t)
                              (let ((*system* this))
                                (open-latch latch)
                                (log:debug "Host main loop running")
                                (loop while enabled-p
                                      do (log-errors
                                           (%glfw:wait-events)
                                           (drain task-queue)))))
                         (%glfw:destroy-window *window*))))
                   (log:debug "Main loop stopped. Host system offline"))))
          (open-latch latch)))))
  (log:debug "Host system initialized"))


(defmethod discard-system :before ((this host-system))
  (with-slots (enabled-p task-queue) this
    (unless enabled-p
      (error "Host system already disabled"))
    (wait-with-latch (latch)
      (run
       (-> this ()
         (setf enabled-p nil)
         (clearup task-queue)
         (open-latch latch))))
    (log:debug "Stopping main thread runner")
    (stop-main-runner)))


(defun bind-rendering-context (&key (main t))
  (with-slots (window shared) (host)
    (if main
        (%glfw:make-context-current window)
        (%glfw:make-context-current shared))))


(defun release-rendering-context ()
  (%glfw:make-context-current (cffi:null-pointer)))


(defun swap-buffers ()
  (let ((host-sys (host)))
    (with-slots (window) host-sys
      (with-system-lock-held (host-sys)
        (%glfw:swap-buffers window)))))


(defun swap-interval ()
  (with-slots (swap-interval) (host)
    swap-interval))


(defun (setf swap-interval) (value)
  (let ((host-sys (host)))
    (with-slots (swap-interval) host-sys
      (with-system-lock-held (host-sys)
        (setf swap-interval value)
        (%glfw:swap-interval value)))))


(define-system-function (setf viewport-title) host-system (value)
  (with-slots (window) *system*
    ;; some darwin systems go crazy throwing FPE around while setting a title
    (claw:with-float-traps-masked ()
      (%glfw:set-window-title window (format nil "~a" value)))))


(define-system-function viewport-size host-system ()
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-window-size (window-of *system*) (width &) (height &))
    (vec2 width height)))


(define-system-function framebuffer-size host-system ()
  (claw:c-with ((width :int)
                (height :int))
    (%glfw:get-framebuffer-size (window-of *system*) (width &) (height &))
    (vec2 width height)))


(define-system-function (setf viewport-size) host-system (value)
  ;; same as with #'(setf viewport-title)
  ;; some darwin systems go nuts throwing FPE around while setting a size
  (claw:with-float-traps-masked ()
    (%glfw:set-window-size (window-of *system*) (floor (x value)) (floor (y value)))))


(define-system-function cursor-position host-system ()
  (let ((height (y (viewport-size))))
    (claw:c-with ((x :double)
                  (y :double))
      (%glfw:get-cursor-pos (window-of *system*) (x &) (y &))
      (vec2 x (- height y)))))


(define-system-function mouse-button-state host-system (button)
  (glfw-enumval->button-state
   (%glfw:get-mouse-button (window-of *system*) (mouse-button->glfw-enumval button))))


(define-system-function lock-cursor host-system ()
  (with-slots (window) *system*
    (%glfw:set-input-mode window %glfw:+cursor+ %glfw:+cursor-disabled+)))


(define-system-function unlock-cursor host-system ()
  (with-slots (window) *system*
    (%glfw:set-input-mode window %glfw:+cursor+ %glfw:+cursor-normal+)))


(define-system-function (setf fullscreen-viewport-p) host-system (value)
  (with-slots (window) *system*
    (if value
        (let* ((monitor (%glfw:get-primary-monitor)))
          (claw:c-let ((video-mode %glfw:vidmode :from (%glfw:get-video-mode monitor)))
            (%glfw:set-window-monitor window monitor 0 0
                                      (video-mode :width)
                                      (video-mode :height)
                                      (video-mode :refresh-rate))))
        (%glfw:set-window-monitor window (cffi:null-pointer) 100 100 640 480 %glfw:+dont-care+))))
