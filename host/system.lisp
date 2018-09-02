(cl:in-package :cl-bodge.host)

(declaim (special *window*))

(define-constant +expected-dpi+ 96)


(defclass host-system (enableable dispatching generic-system bodge-host:application)
  ((shared :initform nil)))


(definline host ()
  (engine-system 'host-system))


(defmacro for-host ((&optional arg) &body body)
  `(flow:-> (host) (,@(when arg (list arg)))
     ,@body))


(defmethod dispatch ((this host-system) (fn function) invariant &key)
  (bodge-host:progm
    (let ((*system* this))
      (funcall fn)))
  t)


(defmethod bodge-host:on-init ((this host-system))
  (with-slots (shared) this
    (setf shared (bodge-host:make-shared-rendering-context this))
    (log:debug "Host system initialized")))


(defmethod bodge-host:on-destroy ((this host-system))
  (with-slots (shared) this
    (bodge-host:destroy-shared-rendering-context shared))
    (log:debug "Host system offline"))


(defmethod bodge-host:on-hide ((this host-system))
  (post 'viewport-hiding-event))


(defmethod bodge-host:on-key-action ((this host-system) key state)
  (post 'keyboard-event :key key :state state))


(defmethod bodge-host:on-mouse-action ((this host-system) button state)
  (post 'mouse-event :button button :state state))


(defmethod bodge-host:on-cursor-movement ((this host-system) x y)
  (post 'cursor-event :x x :y y))


(defmethod bodge-host:on-scroll ((this host-system) x y)
  (post 'scroll-event :x-offset x :y-offset y))


(defmethod bodge-host:on-framebuffer-size-change ((this host-system) w h)
  (post 'framebuffer-size-change-event :width w :height h))


(defmethod bodge-host:on-viewport-size-change ((this host-system) w h)
  (post 'viewport-size-change-event :width w :height h))


(defmethod bodge-host:on-character-input ((this host-system) character)
  (post 'character-input-event :character character))


(defmethod enabling-flow ((this host-system))
  (flow:>> (call-next-method)
           (instantly ()
             (bodge-host:start-application this))))


(defmethod disabling-flow ((this host-system))
  (flow:>> (instantly ()
             (bodge-host:stop-application this))
           (call-next-method)))


(defun bind-rendering-context (&key (main t))
  (let ((host (host)))
    (if main
        (bodge-host:bind-main-rendering-context host)
        (with-slots (shared) host
          (bodge-host:bind-shared-rendering-context shared)))))


(defun release-rendering-context ()
  (bodge-host:release-rendering-context))


(defun swap-buffers ()
  (bodge-host:swap-buffers (host)))


(defun swap-interval ()
  (bodge-host:swap-interval))


(defun (setf swap-interval) (value)
  (setf (bodge-host:swap-interval) value))


(define-system-function (setf viewport-title) host-system (value)
  (setf (bodge-host:viewport-title *system*) value))


(define-system-function viewport-size host-system ()
  (bodge-host:viewport-size *system*))


(define-system-function framebuffer-size host-system ()
  (bodge-host:framebuffer-size *system*))


(define-system-function (setf viewport-size) host-system (value)
  (setf (bodge-host:viewport-size *system*) value))


(define-system-function cursor-position host-system (&optional (result-vec (vec2)))
  (bodge-host:cursor-position *system* result-vec))


(define-system-function mouse-button-state host-system (button)
  (bodge-host:mouse-button-state *system* button))


(define-system-function keyboard-button-state host-system (button)
  (bodge-host:keyboard-button-state *system* button))


(define-system-function lock-cursor host-system ()
  (bodge-host:lock-cursor *system*))


(define-system-function unlock-cursor host-system ()
  (bodge-host:unlock-cursor *system*))


(define-system-function viewport-scale host-system ()
  (bodge-host:viewport-scale *system*))


(define-system-function (setf fullscreen-viewport-p) host-system (value)
  (setf (bodge-host:fullscreen-viewport-p *system*) value))
