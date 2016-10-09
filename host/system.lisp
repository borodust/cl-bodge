(in-package :cl-bodge.host)

(defclass host-system (system)
  ((enabled-p :initform nil)
   (state-lock :initform (make-lock "host-sys-state-lock"))
   (state-condi-var :initform (make-condition-variable
                               :name "host-sys-state-condi-var"))
   (window :initform nil)
   (eve-sys :initform nil :reader event-system-of)
   (job-queue :initform (make-job-queue)))
  (:default-initargs :dependencies '(event-system)))


(defmacro within-main-thread-of ((host-system) &body body)
  (with-gensyms (queue)
    `(with-slots ((,queue job-queue)) ,host-system
       (push-body-into (,queue)
         ,@body)
       (glfw:post-empty-event))))


(glfw:def-window-close-callback on-close (window)
  (glfw:hide-window window))


(glfw:def-key-callback on-key-action (window key scancode action mod-keys)
  (declare (special *host*) (ignore window scancode mod-keys))
  (post (make-keyboard-event (glfw-enumval->keyboard-key key)
                             (glfw-enumval->button-state action))
        (event-system-of *host*)))


(glfw:def-mouse-button-callback on-mouse-action (window button action mod-keys)
  (declare (special *host*) (ignore window mod-keys))
  (post (make-mouse-event (glfw-enumval->mouse-button button)
                          (glfw-enumval->button-state action))
        (event-system-of *host*)))


(glfw:def-cursor-pos-callback on-cursor-movement (window x y)
  (declare (special *host*) (ignore window))
  (post (make-cursor-event x y)
        (event-system-of *host*)))


(glfw:def-scroll-callback on-scroll (window x y)
  (declare (special *host*) (ignore window))
  (post (make-scroll-event x y)
        (event-system-of *host*)))


(glfw:def-framebuffer-size-callback on-framebuffer-size-change (window w h)
  (declare (special *host*) (ignore window))
  (post (make-framebuffer-size-change-event w h) (event-system-of *host*)))


(defun %register-event-classes ()
  (register-event-classes (engine-system 'event-system)
                          'keyboard-event
                          'mouse-event
                          'cursor-event
                          'scroll-event
                          'framebuffer-size-change-event))


;; if current thread is the main one, this function will block
(defmethod enable ((this host-system))
  (with-slots (enabled-p job-queue window state-lock state-condi-var eve-sys) this
    (with-lock-held (state-lock)
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
            (with-lock-held (state-lock)
              (setf window glfw:*window*
                    eve-sys (engine-system 'event-system)
                    enabled-p t))
            (condition-notify state-condi-var)
            (log:debug "Host main loop running")
            (let ((*host* this))
              (declare (special *host*))
              (loop while enabled-p
                 do (handler-case
                        (progn
                          (glfw:wait-events)
                          (drain job-queue))
                      (t (e) (log:error "Unhandled error in the event-loop: ~a" e))))
              (condition-notify state-condi-var)))
          (log:debug "Main loop stopped. Host system offline")))
      (loop until enabled-p do
           (condition-wait state-condi-var state-lock)))))



(defmethod disable ((this host-system))
  (with-slots (enabled-p state-lock state-condi-var) this
    (with-lock-held (state-lock)
      (unless enabled-p
        (error "Host system already disabled"))
      (within-main-thread-of (this)
        (with-slots (enabled-p) this
          (with-lock-held (state-lock)
            (setf enabled-p nil))))
      (loop while enabled-p do
           (condition-wait state-condi-var state-lock)))))


(defun bind-rendering-context (host-sys)
  (with-slots (window state-lock) host-sys
    (with-lock-held (state-lock)
      (glfw:make-context-current window))))


(defun swap-buffers (host-sys)
  (with-slots (window state-lock) host-sys
    (with-lock-held (state-lock)
      (glfw:swap-buffers window))))
