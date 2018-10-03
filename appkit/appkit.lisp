(cl:in-package :cl-bodge.appkit)


(declaim (special *font*))

(defvar +origin+ (vec2 0.0 0.0))
(defvar *black* (vec4 0 0 0 1))
(defvar *window-class-list* (mt:make-guarded-reference nil))
(defvar *appkit-instance-class* nil)

(defvar *default-viewport-width* 640)
(defvar *default-viewport-height* 480)
(defvar *default-viewport-title* "Bodge Appkit")


(defclass appkit-system (enableable generic-system)
  ((framebuffer-size :initform (vec2 640 480) :accessor %framebuffer-size-of)
   (viewport-width :initform *default-viewport-width*)
   (viewport-height :initform *default-viewport-height*)
   (updated-p :initform nil)
   (canvas :initform nil :reader canvas-of)
   (font :initform nil :reader font-of)
   (ui :initform nil :reader ui-of)
   (input-source :initform nil)
   (action-queue :initform (make-task-queue))
   (injected-flows :initform nil))
  (:default-initargs :depends-on '(ge.host:host-system ge.gx:graphics-system)))


(defgeneric %app-configuration-flow (appkit)
  (:method ((this appkit-system)) (declare (ignore this))))


(defgeneric configuration-flow (appkit)
  (:method ((this appkit-system)) (declare (ignore this))))


(defgeneric sweeping-flow (appkit)
  (:method ((this appkit-system)) (declare (ignore this))))


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
                                 :windows))
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


(defun update-graphics (this viewport-width viewport-height window-classes)
  (with-slots (canvas ui) this
    (ge.vg:update-canvas-size canvas viewport-width viewport-height)
    (ge.ui:update-ui-size ui viewport-width viewport-height)
    (ge.ui:with-ui-access (ui)
      (ge.ui:remove-all-windows ui)
      (dolist (window-class window-classes)
        (ge.ui:add-window ui window-class)))
      (initialize-user-interface this)
    (ge.ui:compose-ui ui)))


(defun %app-update-flow (app viewport-title viewport-width viewport-height
                         fullscreen-p window-classes)
  (let ((width (or viewport-width *default-viewport-width*))
        (height (or viewport-height *default-viewport-height*)))
    (>> (ge.host:for-host ()
          (update-viewport app
                           (or viewport-title *default-viewport-title*)
                           width height fullscreen-p))
        (ge.gx:for-graphics ()
          (update-graphics app width height window-classes))
        (configuration-flow app))))


(defmacro defapp (name (&rest classes) &body ((&rest slots) &rest opts))
  (multiple-value-bind (std-opts extended) (split-opts opts)
    `(progn
       (defclass ,name (appkit-system ,@classes)
         ,slots
         ,@std-opts)
       ,(with-hash-entries ((viewport-width :viewport-width)
                            (viewport-height :viewport-height)
                            (viewport-title :viewport-title)
                            (fullscreen-p :fullscreen-p)
                            (windows :windows))
            (alist-hash-table extended)
          `(defmethod %app-configuration-flow ((this ,name))
             (%app-update-flow this
                               ,(first viewport-title)
                               ,(first viewport-width)
                               ,(first viewport-height)
                               ,(first fullscreen-p)
                               (list ,@windows))))
       (make-instances-obsolete ',name))))


(defmethod initialize-instance :around ((this appkit-system) &key)
  (when (null *appkit-instance-class*)
    (error "Manual appkit instance creation forbidden. Use #'appkit:start"))
  (call-next-method))


(defun app ()
  (when *appkit-instance-class*
    (ge.ng:engine-system *appkit-instance-class*)))


(defgeneric act (system)
  (:method ((system appkit-system)) (declare (ignore system))))


(defgeneric draw (system)
  (:method ((system appkit-system)) (declare (ignore system))))


(defgeneric initialize-user-interface (system)
  (:method ((system appkit-system)) (declare (ignore system))))


(defmacro when-app ((appkit-var) &body body)
  `(when-let ((,appkit-var (app)))
     ,@body))


(defun push-action (app action)
  (with-slots (action-queue) app
    (push-task action action-queue)))


(defmethod dispatch ((this appkit-system) (task function) invariant &key)
  (declare (ignore invariant))
  (push-action this task))


(defun inject-flow (flow)
  (when-let ((app-instance (app)))
    (flet ((%inject-flow ()
             (with-slots (injected-flows) app-instance
               (push flow injected-flows))))
      (push-action app-instance #'%inject-flow))))


(define-event-handler on-framebuffer-change ((ev ge.host:framebuffer-size-change-event) width height)
  (when-let ((appkit (app)))
    (flet ((update-framebuffer ()
             (setf (%framebuffer-size-of appkit) (vec2 width height))))
      (push-action appkit #'update-framebuffer))))


(ge.vg:defcanvas appkit-canvas (appkit-instance)
  (draw appkit-instance))


(defun %initialize-graphics (this pixel-ratio)
  (with-slots (viewport-width viewport-height canvas font ui input-source) this
    (setf canvas (ge.vg:make-canvas 'appkit-canvas viewport-width
                                    viewport-height
                                    :pixel-ratio pixel-ratio
                                    :antialiased nil)
          font (ge.vg:make-default-font)
          input-source (ge.ui:make-host-input-source)
          ui (ge.ui:make-ui viewport-width viewport-height :pixel-ratio pixel-ratio
                                                           :input-source input-source
                                                           :antialiased nil)
          (ge.host:swap-interval) 1)
    (ge.ui:attach-host-input-source input-source)))


(defun draw-app (this)
  (with-slots (ui canvas font framebuffer-size) this
    (gl:viewport 0 0 (x framebuffer-size) (y framebuffer-size))
    (gl:clear :color-buffer :depth-buffer :stencil-buffer)
    (let ((*font* font))
      (ge.gx:render t canvas :appkit-instance this)
      (ge.ui:compose-ui ui))
    (ge.host:swap-buffers)))


(defun %app-loop (this)
  (with-slots (action-queue ui canvas font updated-p injected-flows) this
    (labels ((%act ()
               (drain action-queue)
               (act this)))
      (loop-flow (>> (->> ()
                       (when updated-p
                         (setf updated-p nil)
                         (>> (sweeping-flow this)
                             (%app-configuration-flow this))))
                     (->> ()
                       (when injected-flows
                         (prog1 (nreverse injected-flows)
                           (setf injected-flows nil))))
                     (instantly () (%act))
                     (ge.gx:for-graphics () (draw-app this)))
                 (lambda () (enabledp this))))))


(defmethod enabling-flow list ((this appkit-system))
  (>> (ge.host:for-host ()
        (viewport-pixel-ratio))
      (ge.gx:for-graphics (pixel-ratio)
        (%initialize-graphics this pixel-ratio))
      (%app-configuration-flow this)
      (instantly ()
        (run (>> (%app-loop this)
                 (->> () (sweeping-flow this)))))))

;;;
;;; Startup routines
;;;
(defun start (classname &key (log-level :info) (opengl-version '(3 3)) blocking)
  (when *appkit-instance-class*
    (error "Only one active system of type 'appkit-system is allowed"))
  (setf *appkit-instance-class* classname)
  (startup `(:engine (:systems (,classname) :log-level ,log-level)
             :host (:opengl-version ,opengl-version))
           :blocking blocking))


(defun %stop ()
  (when *appkit-instance-class*
    (unwind-protect
         (shutdown)
      (setf *appkit-instance-class* nil))))


(defun stop (&key blocking)
  (if blocking
      (%stop)
      (in-new-thread ("exit-thread")
        (%stop))))


(define-event-handler on-exit ((ev ge.host:viewport-hiding-event))
  (%stop))
