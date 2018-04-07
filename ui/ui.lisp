(cl:in-package :cl-bodge.ui)


(declaim (special *context*
                  *handle*))


(defhandle nuklear-font-handle
    :closeform (nuklear:destroy-user-font *handle-value*))


(defclass nuklear-font (foreign-object) ())


(defun make-nuklear-font (font-line-height font-width-callback-name)
  (make-instance 'nuklear-font
                 :handle (make-nuklear-font-handle
                          (nuklear:make-user-font font-line-height
                                                  font-width-callback-name))))


(defhandle nuklear-context-handle
  :closeform (nuklear:destroy-context *handle-value*))

;;;
;;;
;;;
(defclass nuklear-context (foreign-object)
  ((width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (canvas :initarg :canvas :reader canvas-of)
   (compose-tasks :initform (make-task-queue))
   (input-source :initform nil :initarg :input-source :reader %input-source-of)
   (style-stack :initform nil)
   (last-cursor-position :initform (vec2) :reader %last-cursor-position-of)
   (last-scroll :initform (vec2) :reader %last-scroll-of)
   (nuklear-font :initarg :nuklear-font :reader font-of)
   (last-window-id :initform 0)
   (last-custom-widget-id :initform 0)
   (windows :initform nil :accessor %windows-of)
   (custom-widget-table :initform (make-hash-table))))


(define-destructor nuklear-context (nuklear-font canvas)
  (dispose nuklear-font)
  (dispose canvas))


(defun calc-text-width (text)
  (canvas-text-advance text (canvas-of *context*)))


(nuklear:define-text-width-callback calc-string-width (handle height string)
  (calc-text-width string))


(defmethod initialize-instance ((this nuklear-context) &rest keys &key width height
                                                                    antialiased
                                                                    pixel-ratio)
  (let* ((canvas (make-canvas width height :antialiased antialiased
                                           :pixel-ratio pixel-ratio))
         (nk-font (make-nuklear-font (canvas-font-line-height (canvas-font-metrics canvas))
                                     'calc-string-width)))
    (apply #'call-next-method this
           :handle (make-nuklear-context-handle
                    (nuklear:make-context (handle-value-of nk-font)))
           :canvas canvas
           :nuklear-font nk-font
           keys)))


(definline make-ui (width height &key (antialiased t) (pixel-ratio 1.0) input-source)
  (make-instance 'nuklear-context
                 :input-source input-source
                 :width width
                 :height height
                 :pixel-ratio pixel-ratio
                 :antialiased antialiased))


(defun %add-window (ui window)
  (push window (%windows-of ui))
  window)


(defun %remove-window (ui window)
  (deletef (%windows-of ui) window)
  (%nk:window-close (handle-value-of ui) (%panel-id-of window)))


(defun %next-panel-id ()
  (with-slots (last-window-id) *context*
    (format nil "~A" (incf last-window-id))))


(defun %next-custom-widget-id ()
  (with-slots (last-custom-widget-id) *context*
    (incf last-custom-widget-id)))


(defun context-custom-widget (id &optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (gethash id custom-widget-table)))


(defun (setf context-custom-widget) (value id &optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (setf (gethash id custom-widget-table) value)))


(defun push-compose-task (ctx fn)
  (with-slots (compose-tasks) ctx
    (push-task fn compose-tasks)))


(defmacro with-ui-access ((ctx) &body body)
  `(push-compose-task ,ctx (lambda () ,@body)))


(defmacro with-ui ((ctx) &body body)
  `(let ((*context* ,ctx)
         (*handle* (handle-value-of ,ctx)))
     ,@body))


(defun drain-compose-task-queue (ctx)
  (with-slots (compose-tasks) ctx
    (with-ui (ctx)
      (drain compose-tasks))))


(defmacro with-ui-input ((ui) &body body)
  `(with-ui (,ui)
     (prog2
         (%nk:input-begin *handle*)
         (progn ,@body)
       (%nk:input-end *handle*))))


(definline clear-ui (&optional (ui *context*))
  (with-slots (custom-widget-table) ui
    (clrhash custom-widget-table))
  (%nk:clear (handle-value-of ui)))


(defun register-cursor-position (x y)
  (%nk:input-motion *handle* (floor x) (floor (- (height-of *context*) y))))


(defun register-character-input (character)
  (%nk:input-unicode *handle* (char-code character)))


(defun register-scroll-input (x y)
  (c-with ((vec (:struct (%nk:vec2))))
    (setf (vec :x) (float x 0f0)
          (vec :y) (float y 0f0))
    (%nk:input-scroll *handle* vec)))


(defun update-ui-size (ui width height)
  (with-slots ((w width) (h height) text-renderer canvas) ui
    (update-canvas-size canvas width height)
    (setf w width
          h height)))


(defun button-state->nk (state)
  (ecase state
    (:pressed %nk:+true+)
    (:released %nk:+false+)))


(defvar *nk-key-map*
  (make-hash-table-with-entries ()
    (:left-shift %nk:+key-shift+)
    (:right-shift %nk:+key-shift+)
    (:left-control %nk:+key-ctrl+)
    (:right-control %nk:+key-ctrl+)
    (:delete %nk:+key-del+)
    (:enter %nk:+key-enter+)
    (:tab %nk:+key-tab+)
    (:backspace %nk:+key-backspace+)
    (:up %nk:+key-up+)
    (:down %nk:+key-down+)
    (:left %nk:+key-left+)
    (:right %nk:+key-right+)))


(defun key->nk (key)
  (gethash key *nk-key-map* %nk:+key-none+))


(defun register-keyboard-input (key state)
  (if (eq state :repeating)
      (progn
        (register-keyboard-input key :released)
        (register-keyboard-input key :pressed))
      (%nk:input-key *handle* (key->nk key) (button-state->nk state))))


(defun register-mouse-input (x y button state)
  (let ((nk-button (ecase button
                     (:left %nk:+button-left+)
                     (:middle %nk:+button-middle+)
                     (:right %nk:+button-right+)))
        (nk-state (button-state->nk state)))
    (%nk:input-button *handle* nk-button
                      (floor x) (floor (- (height-of *context*) y)) nk-state)))

;;;
;;;
;;;
(defhandle style-item-handle
    :initform (alloc '(:struct (%nk:style-item)))
    :closeform (free *handle-value*))


(defclass style-item (foreign-object) ()
  (:default-initargs :handle (make-style-item-handle)))


(defclass color-style-item (style-item) ())


(defun style-item-color (style-item color)
  (c-with ((color-v (:struct (%nk:color))))
    (%nk:style-item-color style-item
                          (%nk:rgba-f color-v (x color) (y color) (z color) (w color)))))

(defmethod initialize-instance :after ((this color-style-item)
                                       &key (color (error ":color missing")))
  (style-item-color (handle-value-of this) color))


;;;
;;;
;;;

(defun push-style-popper (fu context)
  (with-slots (style-stack) context
    (push fu style-stack)))


(defun pop-style (context)
  (with-slots (style-stack) context
    (when-let ((pop-fu (pop style-stack)))
      (funcall pop-fu (handle-value-of context)))))


(defgeneric push-style (context destination source))


(defmethod push-style ((context nuklear-context) destination (vec vec2))
  (c-with ((vec-v (:struct (%nk:vec2))))
    (setf (vec-v :x) (x vec)
          (vec-v :y) (y vec))
    (%nk:style-push-vec2 (handle-value-of context) destination vec-v)
    (push-style-popper #'%nk:style-pop-vec2 context)))

(defmethod push-style ((context nuklear-context) destination (value single-float))
  (%nk:style-push-float (handle-value-of context) destination value)
  (push-style-popper #'%nk:style-pop-float context))

(defmethod push-style ((context nuklear-context) destination (item style-item))
  (%nk:style-push-style-item (handle-value-of context) destination (handle-value-of item))
  (push-style-popper #'%nk:style-pop-style-item context))

(defun %nopper (handle)
  (declare (ignore handle)))

(defmethod push-style ((context nuklear-context) destination (item null))
  (push-style-popper #'%nopper context))
