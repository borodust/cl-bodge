(cl:in-package :cl-bodge.host)

(declaim (special *window*))

(defvar *gamepad-mappings*
  (bodge-util:read-file-into-string
   (asdf:system-relative-pathname :cl-bodge/host "host/gamecontrollerdb.txt")))


(define-constant +expected-dpi+ 96)

(defclass host-application (bodge-host:window)
  ((init-continuation :initarg :init-continuation)
   (destroy-continuation :initarg :destroy-continuation)))


(defun set-destroy-continuation (host-application cont)
  (with-slots (destroy-continuation) host-application
    (setf destroy-continuation cont)))


(defclass host-system (enableable dispatching generic-system)
  ((host-application :initform nil)))


(definline host-application (&optional (system *system*))
  (slot-value system 'host-application))


(definline host ()
  (engine-system 'host-system))


(defmacro for-host (&body arguments-and-body)
  (multiple-value-bind (initargs arg-and-body)
      (parse-initargs-and-list arguments-and-body)
    `(-> (host) ,@initargs ,(first arg-and-body)
       ,@(rest arg-and-body))))


(defmethod dispatch ((this host-system) (fn function) invariant &key)
  (bodge-host:progm
    (let ((*system* this))
      (funcall fn)))
  t)


(defmethod bodge-host:on-controller-connect ((this host-application) controller)
  (post 'controller-connected-event :controller controller))


(defmethod bodge-host:on-controller-disconnect ((this host-application) controller)
  (post 'controller-disconnected-event :controller controller))


(defmethod bodge-host:on-gamepad-connect ((this host-application) gamepad)
  (post 'gamepad-connected-event :gamepad gamepad))


(defmethod bodge-host:on-gamepad-disconnect ((this host-application) gamepad)
  (post 'gamepad-disconnected-event :gamepad gamepad))


(defmethod bodge-host:on-gamepad-action ((this host-application) gamepad button state)
  (post 'gamepad-button-event :gamepad gamepad :button button :state state))


(defmethod bodge-host:on-dpad-action ((this host-application) gamepad state)
  (post 'gamepad-dpad-event :gamepad gamepad :state state))


(defmethod bodge-host:on-left-stick-movement ((this host-application)
                                              gamepad
                                              x y)
  (post 'gamepad-left-stick-event :gamepad gamepad :x x :y y))


(defmethod bodge-host:on-right-stick-movement ((this host-application)
                                              gamepad
                                              x y)
  (post 'gamepad-right-stick-event :gamepad gamepad :x x :y y))


(defmethod bodge-host:on-left-trigger ((this host-application)
                                              gamepad
                                              value)
  (post 'gamepad-left-trigger-event :gamepad gamepad :value value))


(defmethod bodge-host:on-right-trigger ((this host-application)
                                        gamepad
                                        value)
  (post 'gamepad-right-trigger-event :gamepad gamepad :value value))


(defmethod bodge-host:on-init ((this host-application))
  (with-slots (init-continuation) this
    (bodge-host:update-gamepad-mappings *gamepad-mappings*)
    (run (concurrently ()
           (funcall init-continuation)))))


(defmethod bodge-host:on-destroy ((this host-application))
  (with-slots (destroy-continuation) this
    (run (concurrently ()
           (funcall destroy-continuation)))))


(defmethod bodge-host:on-hide ((this host-application))
  (post 'viewport-hiding-event))


(defun collect-modifiers (app)
  (loop for mod in '(:shift :control :alt :super :caps-lock :num-lock)
        when (bodge-host:modifiers-engaged-p app mod)
          collect mod))


(defmethod bodge-host:on-key-action ((this host-application) key state)
  (post 'keyboard-event :key key :state state :modifiers (collect-modifiers this)))


(defmethod bodge-host:on-mouse-action ((this host-application) button state)
  (post 'mouse-event :button button :state state :modifiers (collect-modifiers this)))


(defmethod bodge-host:on-cursor-movement ((this host-application) x y)
  (post 'cursor-event :x x :y y))


(defmethod bodge-host:on-scroll ((this host-application) x y)
  (post 'scroll-event :x-offset x :y-offset y))


(defmethod bodge-host:on-framebuffer-size-change ((this host-application) w h)
  (post 'framebuffer-size-change-event :width w :height h))


(defmethod bodge-host:on-viewport-size-change ((this host-application) w h)
  (post 'viewport-size-change-event :width w :height h))


(defmethod bodge-host:on-character-input ((this host-application) character)
  (post 'character-input-event :character character :modifiers (collect-modifiers this)))


(defun make-host-application (cont)
  (make-instance 'host-application
                 :init-continuation cont
                 :opengl-version (property '(:host :opengl-version) '(3 3))
                 :resizable (property '(:host :viewport-resizable) nil)
                 :decorated (property '(:host :viewport-decorated) t)
                 :transparent (property '(:host :viewport-transparent) nil)
                 :samples (property '(:host :samples) nil)
                 :autoscaled (property '(:host :autoscaled) t)))


(defmethod enabling-flow list ((this host-system))
  (with-slots (host-application) this
    (>> (%> ()
          (setf host-application (make-host-application #'ge.ng:continue-flow))
          (bodge-host:open-window (host-application this)))
        (instantly ()
          (log/debug "Host system initialized")))))


(defmethod disabling-flow list ((this host-system))
  (>> (%> ()
        (set-destroy-continuation (host-application this) #'continue-flow)
        (bodge-host:close-window (host-application this)))
      (instantly ()
        (log/debug "Host system offline"))))


(defclass shared-context (disposable)
  ((handle :initarg :handle :reader %handle-of)))


(define-destructor shared-context (handle)
  (run (for-host :disposing t ()
         (bodge-host:destroy-shared-rendering-context handle))))


(define-system-function make-shared-rendering-context host-system ()
  (make-instance 'shared-context
                 :handle (bodge-host:make-shared-rendering-context (host-application (host)))))


(defun bind-rendering-context (&optional context)
  (if context
      (bodge-host:bind-shared-rendering-context (%handle-of context))
      (bodge-host:bind-main-rendering-context (host-application (host)))))


(defun release-rendering-context ()
  (bodge-host:release-rendering-context))


(defun swap-buffers ()
  (bodge-host:swap-buffers (host-application (host))))


(defun swap-interval ()
  (bodge-host:swap-interval))


(defun (setf swap-interval) (value)
  (setf (bodge-host:swap-interval) value))


(define-system-function (setf viewport-title) host-system (value)
  (setf (bodge-host:viewport-title (host-application)) value))


(define-system-function viewport-size host-system ()
  (bodge-host:viewport-size (host-application)))


(defmacro with-viewport-dimensions ((width height) &body body)
  `(bodge-host:with-viewport-dimensions (,width ,height) (host-application)
     ,@body))


(define-system-function framebuffer-size host-system ()
  (bodge-host:framebuffer-size (host-application)))


(defmacro with-framebuffer-dimensions ((width height) &body body)
  `(bodge-host:with-framebuffer-dimensions (,width ,height) (host-application)
     ,@body))


(define-system-function (setf viewport-size) host-system (value)
  (setf (bodge-host:viewport-size (host-application)) value))


(define-system-function cursor-position host-system (&optional (result-vec (vec2)))
  (bodge-host:cursor-position (host-application) result-vec))


(define-system-function mouse-button-state host-system (button)
  (bodge-host:mouse-button-state (host-application) button))


(define-system-function keyboard-button-state host-system (button)
  (bodge-host:keyboard-button-state (host-application) button))


(define-system-function lock-cursor host-system ()
  (bodge-host:lock-cursor (host-application)))


(define-system-function unlock-cursor host-system ()
  (bodge-host:unlock-cursor (host-application)))


(define-system-function viewport-scale host-system ()
  (bodge-host:viewport-scale (host-application)))


(define-system-function (setf fullscreen-viewport-p) host-system (value)
  (setf (bodge-host:fullscreen-viewport-p (host-application)) value))


(define-system-function refresh-rate host-system ()
  (let* ((monitor (bodge-host:window-monitor (host-application)))
         (video-mode (bodge-host:monitor-video-mode monitor)))
    (bodge-host:video-mode-refresh-rate video-mode)))
