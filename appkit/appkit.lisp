(cl:in-package :cl-bodge.appkit)


(declaim (special *font*))

(define-constant +default-frame-rate+ 60)
(defvar +origin+ (vec2 0.0 0.0))
(defvar +black+ (vec4 0 0 0 1))

(defvar *appkit-instance-class* nil)
(defvar *appkit-instance* nil)

(define-constant +double-float-drift-time-correction+ 0.0000001d0
  :test #'=)

(defparameter *default-viewport-width* 800)
(defparameter *default-viewport-height* 600)
(defparameter *default-viewport-title* "Bodge Appkit")


(defclass appkit-system (enableable generic-system)
  ((framebuffer-size :initform (vec2 *default-viewport-width*
                                     *default-viewport-height*)
                     :accessor %framebuffer-size-of)
   (updated-p :initform nil)
   (canvas :initform nil)
   (font :initform nil)
   (ui :initform nil)
   (input-source :initform nil)
   (action-queue :initform (make-task-queue))
   (injected-flows :initform nil)
   (disabled-p :initform nil)
   (sweep-continuation :initform nil)
   (canvas-width :initform nil)
   (canvas-height :initform nil)
   (frame-queue :initform nil)))


(defgeneric %app-configuration-flow (appkit)
  (:method (this) (declare (ignore this))))


(defgeneric configuration-flow (appkit)
  (:method (this) (declare (ignore this))))


(defgeneric sweeping-flow (appkit)
  (:method (this) (declare (ignore this))))


(defgeneric acting-flow (appkit)
  (:method (this) (declare (ignore this))))


(defmethod update-instance-for-redefined-class :after ((this appkit-system)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (updated-p) this
    (setf updated-p t)))


(defun split-opts (opts)
  (loop for opt in opts
        if (member (first opt) '(:viewport-width
                                 :viewport-height
                                 :viewport-title
                                 :fullscreen-p
                                 :panels
                                 :depends-on
                                 :default-initargs
                                 :canvas-width
                                 :canvas-height
                                 :draw-rate
                                 :act-rate))
          collect opt into extended
        else
          collect opt into std
        finally (return (values std extended))))


(defun viewport-pixel-ratio ()
  (let* ((vp-size (ge.host:viewport-size))
         (fb-size (ge.host:framebuffer-size)))
    (/ (x fb-size) (x vp-size))))


(defun update-viewport (app viewport-title viewport-width viewport-height fullscreen-p)
  (with-slots (framebuffer-size) app
    (setf (ge.host:viewport-title) viewport-title
          (ge.host:fullscreen-viewport-p) fullscreen-p
          (ge.host:viewport-title) viewport-title
          (ge.host:viewport-size) (vec2 viewport-width viewport-height))
    (let ((pixel-ratio (viewport-pixel-ratio)))
      (setf framebuffer-size (vec2 (* viewport-width pixel-ratio)
                                   (* viewport-height pixel-ratio))))))


(defun update-frame-queue (app draw-rate act-rate)
  (with-slots (frame-queue) app
    (setf frame-queue (muth:make-blocking-timed-queue))
    (let* ((current-time (real-time-seconds))
           (draw-item (cons (%draw-flow app)
                            (cons current-time
                                  (/ 1 (or draw-rate +default-frame-rate+)))))
           (act-item (cons (%act-flow app)
                           (cons (- current-time single-float-epsilon)
                                 (/ 1 (or act-rate +default-frame-rate+))))))
      (%reschedule-flow frame-queue draw-item)
      (%reschedule-flow frame-queue act-item))))


(defun update-graphics (this viewport-width viewport-height
                        canvas-width canvas-height panel-classes)
  (with-slots (canvas ui
               (this-canvas-width canvas-width)
               (this-canvas-height canvas-height))
      this
    (setf this-canvas-width canvas-width
          this-canvas-height canvas-height)
    (ge.vg:update-canvas-size canvas
                              (or this-canvas-width viewport-width)
                              (or this-canvas-height viewport-height))
    (ge.ui:update-ui-size ui viewport-width viewport-height)
    (ge.ui:with-ui-access (ui)
      (ge.ui:remove-all-panels ui)
      (dolist (panel-class panel-classes)
        (ge.ui:add-panel ui panel-class)))
    (ge.ui:compose-ui ui)))


(defun %app-update-flow (app viewport-title viewport-width viewport-height
                         fullscreen-p canvas-width canvas-height panel-classes
                         draw-rate act-rate)
  (let ((width (or viewport-width *default-viewport-width*))
        (height (or viewport-height *default-viewport-height*)))
    (>> (ge.host:for-host ()
          (log/debug "Updating appkit host configuration")
          (update-viewport app
                           (or viewport-title *default-viewport-title*)
                           width height fullscreen-p))
        (ge.gx:for-graphics ()
          (log/debug "Updating appkit graphics configuration")
          (update-graphics app width height canvas-width canvas-height panel-classes))
        (instantly ()
          (log/debug "Updating framerate")
          (ge.host:swap-buffers)
          (update-frame-queue app draw-rate act-rate))
        (configuration-flow app))))


(defmacro defapp (name (&rest classes) &body ((&rest slots) &rest opts))
  (multiple-value-bind (std-opts extended) (split-opts opts)
    (with-hash-entries ((viewport-width :viewport-width)
                        (viewport-height :viewport-height)
                        (viewport-title :viewport-title)
                        (fullscreen-p :fullscreen-p)
                        (panels :panels)
                        (depends-on :depends-on)
                        (default-initargs :default-initargs)
                        (canvas-width :canvas-width)
                        (canvas-height :canvas-height)
                        (draw-rate :draw-rate)
                        (act-rate :act-rate))
                       (alist-hash-table extended)
      `(progn
         (defclass ,name (appkit-system ,@classes)
           ,slots
           (:default-initargs :%appkit-depends-on ',depends-on ,@default-initargs)
           ,@std-opts)
         (defmethod %app-configuration-flow ((this ,name))
           (%app-update-flow this
                             ,(first viewport-title)
                             ,(first viewport-width)
                             ,(first viewport-height)
                             ,(first fullscreen-p)
                             ,(first canvas-width)
                             ,(first canvas-height)
                             (list ,@panels)
                             ,(first draw-rate)
                             ,(first act-rate)))
         (make-instances-obsolete ',name)))))


(defmethod initialize-instance :around ((this appkit-system) &key)
  (when (null *appkit-instance-class*)
    (error "Manual appkit instance creation forbidden. Use #'appkit:start"))
  (call-next-method))


(defmethod initialize-instance ((this appkit-system) &rest initargs
                                &key %appkit-depends-on depends-on)
  (apply #'call-next-method this
         :depends-on (union (union depends-on %appkit-depends-on)
                            '(ge.host:host-system ge.gx:graphics-system))
         initargs))


(defun app ()
  *appkit-instance*)


(defgeneric draw (system)
  (:method (system) (declare (ignore system))))


(defmethod draw :around ((this appkit-system))
  (let ((*font* (slot-value this 'font)))
    (call-next-method)))


(defmacro when-app ((appkit-var) &body body)
  `(when-let ((,appkit-var (app)))
     ,@body))


(defun app-canvas ()
  (when-app (app)
    (slot-value app 'canvas)))


(defun app-ui ()
  (when-app (app)
    (slot-value app 'ui)))


(defun app-font ()
  (when-app (app)
    (slot-value app 'font)))


(defun push-action (app action)
  (with-slots (action-queue) app
    (push-task action action-queue)))


(defmethod dispatch ((this appkit-system) (task function) invariant &key)
  (declare (ignore invariant))
  (push-action this task))


(defun inject-flow (flow)
  (unless *appkit-instance*
    (error "Cannot inject a flow: appkit system is not started yet"))
  (flet ((%inject-flow ()
           (with-slots (injected-flows) *appkit-instance*
             (push flow injected-flows))))
    (push-action *appkit-instance* #'%inject-flow)))


(define-event-handler on-framebuffer-change ((ev ge.host:framebuffer-size-change-event) width height)
  (when-let ((appkit (app)))
    (flet ((update-framebuffer ()
             (setf (%framebuffer-size-of appkit) (vec2 width height))))
      (push-action appkit #'update-framebuffer))))


(ge.vg:defcanvas appkit-canvas (appkit-instance)
  (draw appkit-instance))


(defun %initialize-graphics (this pixel-ratio)
  (with-slots (canvas font ui input-source canvas-width canvas-height) this
    (let ((antialiased-p (property '(:appkit :antialiased) nil)))
      (setf canvas (ge.vg:make-canvas 'appkit-canvas
                                      (or canvas-width *default-viewport-width*)
                                      (or canvas-height *default-viewport-height*)
                                      :pixel-ratio pixel-ratio
                                      :antialiased antialiased-p)
            font (ge.vg:make-default-font)
            input-source (ge.ui:make-host-input-source)
            ui (ge.ui:make-ui *default-viewport-width*
                              *default-viewport-height*
                              :pixel-ratio pixel-ratio
                              :input-source input-source
                              :antialiased antialiased-p)
            (ge.host:swap-interval) (or (property '(:appkit :swap-interval)) 0)))
    (ge.ui:attach-host-input-source input-source)))


(defgeneric handle-drawing (system canvas ui)
  (:method (system canvas ui)
    (declare (ignore system canvas ui))
    nil))


(defun render-app-canvas (app &optional (rendering-output t))
  (with-slots (canvas) app
    (ge.gx:render rendering-output canvas :appkit-instance app)))


(defun draw-app (this)
  (with-slots (ui canvas) this
    (ge.gx:reset-viewport)
    (unless (handle-drawing this canvas ui)
      (ge.gx:clear-rendering-output t)
      (render-app-canvas this)
      (ge.ui:compose-ui ui))
    (ge.host:swap-buffers)))


(defun %reschedule-flow (queue item)
  (destructuring-bind (expected-time . interval) (cdr item)
    (let* ((current-time (real-time-seconds))
           (adjusted-interval (- interval
                                 (- current-time expected-time
                                    +double-float-drift-time-correction+)))
           (rescheduled-wait (if (< adjusted-interval 0)
                                 (mod adjusted-interval interval)
                                 adjusted-interval))
           (new-expected-time (+ current-time rescheduled-wait)))
      (setf (cadr item) new-expected-time)
      (muth:blocking-timed-queue-push queue item rescheduled-wait))))


(defun %looped-flow (this)
  (with-slots (frame-queue) this
    (->> ()
      (let ((item (muth:blocking-timed-queue-pop frame-queue)))
        (prog1 (car item)
          (%reschedule-flow frame-queue item))))))


(defun %app-loop (this)
  (with-slots (disabled-p sweep-continuation) this
    (>> (loop-flow (%looped-flow this)
                   (lambda () (not disabled-p)))
        (instantly ()
          (log/debug "Appkit loop interrupted")
          (funcall sweep-continuation)))))


(defun %draw-flow (this)
  (>> (ge.gx:for-graphics ()
        (draw-app this))))


(defun %act-flow (this)
  (with-slots (action-queue ui canvas font updated-p injected-flows) this
    (>> (->> ()
          (when updated-p
            (setf updated-p nil)
            (>> (sweeping-flow this)
                (%app-configuration-flow this))))
        (->> ()
          (when injected-flows
            (prog1 (nreverse injected-flows)
              (setf injected-flows nil))))
        (instantly ()
          (drain action-queue))
        (->> ()
          (acting-flow this)))))


(defmethod enabling-flow list ((this appkit-system))
  (>> (ge.host:for-host ()
        (log/debug "Configuring host for appkit")
        (* (viewport-pixel-ratio) (ge.host:viewport-scale)))
      (ge.gx:for-graphics (pixel-ratio)
        (log/debug "Configuring graphics for appkit")
        (%initialize-graphics this pixel-ratio))
      (instantly ()
        (setf *appkit-instance* this))
      (%app-configuration-flow this)
      (instantly ()
        (log/debug "Starting appkit loop")
        (run (%app-loop this)))))


(defmethod disabling-flow list ((this appkit-system))
  (with-slots (sweep-continuation disabled-p canvas ui) this
    (>> (%> ()
          (log/debug "Stopping appkit loop")
          (setf disabled-p t
                sweep-continuation #'continue-flow))
        (->> ()
          (log/debug "Starting sweeping")
          (sweeping-flow this))
        (instantly ()
          (log/debug "Releasing appkit resources")
          (dispose ui)
          (dispose canvas)
          (setf *appkit-instance-class* nil
                *appkit-instance* nil)))))


;;;
;;; Startup routines
;;;
(defun start (classname &key (log-level :info)
                          (opengl-version '(3 3))
                          samples
                          blocking
                          viewport-resizable
                          (viewport-decorated t)
                          (autoscaled t)
                          swap-interval
                          properties)
  (when *appkit-instance-class*
    (error "Only one active system of type 'appkit-system is allowed"))
  (setf *appkit-instance-class* classname)
  (startup (append properties
                   `(:engine (:systems (,classname) :log-level ,log-level)
                     :host (:opengl-version ,opengl-version
                            :samples ,samples
                            :viewport-resizable ,viewport-resizable
                            :viewport-decorated ,viewport-decorated
                            :autoscaled ,autoscaled)
                     :appkit (:antialiased ,(not samples)
                              :swap-interval ,swap-interval)))
           :blocking blocking))


(defun stop (&key blocking)
  (if blocking
      (shutdown)
      (in-new-thread ("exit-thread")
        (shutdown))))


(define-event-handler on-exit ((ev ge.host:viewport-hiding-event))
  (shutdown))
