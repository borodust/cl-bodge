(in-package :cl-bodge.host)


(defclass host-system (dispatcher generic-system)
  ((enabled-p :initform nil)
   (state-condi-var :initform (make-condition-variable
                               :name "host-sys-state-condi-var"))
   (window :initform nil :reader window-of)
   (eve-sys :initform nil :reader event-system-of)
   (task-queue :initform (make-task-queue)))
  (:default-initargs :depends-on '(event-system)))


(definline host ()
  (engine-system 'host-system))


(defmethod enabledp ((this host-system))
  (slot-value this 'enabled-p))


(defmethod dispatch ((this host-system) (fn function) &key)
  (with-slots (task-queue) this
    (with-system-lock-held (this)
      (push-task fn task-queue)
      (glfw:post-empty-event)))
  t)


(glfw:def-window-close-callback on-close (window)
  (glfw:hide-window window)
  (post (make-viewport-hiding-event) (event-system-of *system*)))


(glfw:def-key-callback on-key-action (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (post (make-keyboard-event (glfw-enumval->keyboard-key key)
                             (glfw-enumval->button-state action))
        (event-system-of *system*)))


(glfw:def-mouse-button-callback on-mouse-action (window button action mod-keys)
  (declare (ignore window mod-keys))
  (post (make-mouse-event (glfw-enumval->mouse-button button)
                          (glfw-enumval->button-state action))
        (event-system-of *system*)))


(glfw:def-cursor-pos-callback on-cursor-movement (window x y)
  (declare (ignore window))
  (post (make-cursor-event x y)
        (event-system-of *system*)))


(glfw:def-scroll-callback on-scroll (window x y)
  (declare (ignore window))
  (post (make-scroll-event x y)
        (event-system-of *system*)))


(glfw:def-framebuffer-size-callback on-framebuffer-size-change (window w h)
  (declare (ignore window))
  (post (make-framebuffer-size-change-event w h) (event-system-of *system*)))


(defun %register-event-classes ()
  (register-event-classes (engine-system 'event-system)
                          'keyboard-event
                          'mouse-event
                          'cursor-event
                          'scroll-event
                          'framebuffer-size-change-event
                          'viewport-hiding-event))


;; if current thread is the main one, this function will block
(defmethod initialize-system :after ((this host-system))
  (with-slots (enabled-p task-queue window state-condi-var eve-sys) this
    (with-system-lock-held (this state-lock)
      (when enabled-p
        (error "Host system already enabled"))
      (%register-event-classes)
      (with-body-in-main-thread ()
        (log-errors
          (glfw:with-init-window (:title "Scene" :width 640 :height 480
                                         :context-version-major 4
                                         :context-version-minor 1
                                         :opengl-profile :opengl-core-profile
                                         :opengl-forward-compat t
                                         :depth-bits 32
                                         :samples 4)
            (glfw:set-window-close-callback 'on-close)
            (glfw:set-key-callback 'on-key-action)
            (glfw:set-mouse-button-callback 'on-mouse-action)
            (glfw:set-cursor-position-callback 'on-cursor-movement)
            (glfw:set-scroll-callback 'on-scroll)
            (glfw:set-framebuffer-size-callback 'on-framebuffer-size-change)
            (with-system-lock-held (this)
              (setf window glfw:*window*
                    eve-sys (engine-system 'event-system)
                    enabled-p t))
            (condition-notify state-condi-var)
            (log:debug "Host main loop running")
            (let ((*system* this))
              (loop while enabled-p
                 do (log-errors
                      (glfw:wait-events)
                      (drain task-queue)))
              (condition-notify state-condi-var)))
          (log:debug "Main loop stopped. Host system offline")))
      (loop until enabled-p do
           (condition-wait state-condi-var state-lock)))))


(defmethod discard-system :before ((this host-system))
  (with-slots (enabled-p state-condi-var task-queue) this
    (with-system-lock-held (this state-lock)
      (unless enabled-p
        (error "Host system already disabled"))
      (run
       (-> this ()
         (with-system-lock-held (this)
           (setf enabled-p nil)
           (clearup task-queue))))
      (loop while enabled-p do
           (condition-wait state-condi-var state-lock))
      (stop-main-runner))))


(defun bind-rendering-context (host-sys)
  (with-slots (window) host-sys
    (with-system-lock-held (host-sys)
      (glfw:make-context-current window))))


(defun swap-buffers (host-sys)
  (with-slots (window) host-sys
    (with-system-lock-held (host-sys)
      (glfw:swap-buffers window))))


(define-system-function (setf viewport-title) host-system (value &key (host-sys *system*))
  (with-slots (window) host-sys
    (glfw:set-window-title (format nil "~a" value) window)))


(define-system-function viewport-size host-system ()
  (glfw:get-window-size (window-of *system*)))


(define-system-function (setf viewport-size) host-system (value)
  (destructuring-bind (w h) value
    (glfw:set-window-size w h (window-of *system*))))


(define-system-function cursor-position host-system ()
  (glfw:get-cursor-position (window-of *system*)))


(define-system-function mouse-button-state host-system (button)
  (glfw-enumval->button-state
   (glfw:get-mouse-button (mouse-button->glfw-enumval button) (window-of *system*))))


(define-system-function lock-cursor host-system (&key (host *system*))
  (with-slots (window) host
    (glfw:set-input-mode :cursor :disabled)))


(define-system-function (setf fullscreen-viewport-p) host-system (value)
  (with-slots (window) *system*
    (if value
        (let* ((monitor (glfw:get-primary-monitor))
               (video-mode (%glfw:get-video-mode monitor))
               (width (getf video-mode '%glfw:width))
               (height (getf video-mode '%glfw:height)))
          (glfw:set-window-monitor monitor width height :window window))
        (glfw:set-window-monitor nil 640 480 :window window :x-position 100 :y-position 100))))
