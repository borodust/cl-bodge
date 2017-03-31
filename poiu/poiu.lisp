(in-package :cl-bodge.poiu)


(declaim (special *context*
                  *handle*))


(defhandle nuklear-font-handle
    :closeform (bodge-nuklear:destroy-user-font *handle-value*))


(defclass nuklear-font (foreign-object) ())


(defun make-nuklear-font (font-line-height font-width-callback-name)
  (make-instance 'nuklear-font
                 :handle (make-nuklear-font-handle
                          (bodge-nuklear:make-user-font font-line-height
                                                        font-width-callback-name))))


(defhandle nuklear-context-handle
    :closeform (bodge-nuklear:destroy-context *handle-value*))

;;;
;;;
;;;
(defclass nuklear-context (foreign-object)
  ((nuklear-font :initarg :nuklear-font)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (canvas :initarg :canvas :reader canvas-of)
   (compose-tasks :initform (make-task-queue))
   (text-renderer :initarg :text-renderer :reader text-renderer-of)))


(defmethod font-of ((ctx nuklear-context))
  (with-slots (text-renderer) ctx
    (font-of text-renderer)))


(define-destructor nuklear-context (nuklear-font canvas)
  (dispose nuklear-font)
  (dispose canvas))


(bodge-nuklear:define-font-width-callback calc-string-width (handle height string)
  (let ((w (first (measure-string string (font-of *context*)))))
    (* w (scale-of (text-renderer-of *context*)))))


(defmethod initialize-instance ((this nuklear-context) &rest keys &key width height
                                                                    font line-height
                                                                    antialiased)
  (let ((nk-font (make-nuklear-font line-height 'calc-string-width)))
    (apply #'call-next-method this
           :handle (make-nuklear-context-handle
                    (bodge-nuklear:make-context (handle-value-of nk-font)))
           :canvas (make-canvas width height :antialiased antialiased)
           :nuklear-font nk-font
           :text-renderer (make-text-renderer width height font line-height)
           keys)))


(definline make-poiu-context (width height font line-height &key antialiased)
  (make-instance 'nuklear-context
                 :width width
                 :height height
                 :font font
                 :line-height line-height
                 :antialiased antialiased))


(defun push-compose-task (ctx fn)
  (with-slots (compose-tasks) ctx
    (push-task fn compose-tasks)))


(defmacro when-composing ((ctx) &body body)
  `(push-compose-task ,ctx (lambda () ,@body)))


(defun drain-compose-task-queue (ctx)
  (with-slots (compose-tasks) ctx
    (drain compose-tasks)))


(defmacro with-poiu ((ctx) &body body)
  `(let ((*context* ,ctx)
         (*handle* (handle-value-of ,ctx)))
     ,@body))


(defmacro with-poiu-input ((poiu) &body body)
  `(with-poiu (,poiu)
     (prog2
         (%nk:input-begin *handle*)
         (progn ,@body)
       (%nk:input-end *handle*))))


(definline clear-poiu-context (&optional (poiu *context*))
  (%nk:clear (handle-value-of poiu)))


(defun register-cursor-position (x y)
  (%nk:input-motion *handle* (floor x) (floor (- (height-of *context*) y))))


(defun register-character-input (character)
  (%nk:input-unicode *handle* (char-code character)))


(defun update-poiu-canvas-size (poiu width height)
  (with-slots ((w width) (h height) text-renderer canvas) poiu
    (update-text-renderer-canvas-size text-renderer width height)
    (update-canvas-size canvas width height)
    (setf w width
          h height)))


(defun button-state->nk (state)
  (ecase state
    (:pressed 1)
    (:released 0)))


(defun key->nk (key)
  (ecase key
    (:backspace %nk:+key-backspace+)))


(defun register-keyboard-input (key state)
  (%nk:input-key *handle* (key->nk key) (button-state->nk state)))


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


(defmethod initialize-instance :after ((this color-style-item)
                                       &key (color (error ":color missing")))
  (%nk:bge-init-color-style-item (handle-value-of this) (x color) (y color) (z color) (w color)))


;;;
;;;
;;;
(defgeneric push-style (context destination source))
(defgeneric pop-style (context class))


(defmethod push-style ((context nuklear-context) destination (vec vec2))
  (%nk:bge-style-push-vec2 (handle-value-of context) destination (x vec) (y vec)))

(defmethod pop-style ((context nuklear-context) (class (eql 'vec2)))
  (%nk:style-pop-vec2 (handle-value-of context)))


(defmethod push-style ((context nuklear-context) destination (value single-float))
  (%nk:style-push-float (handle-value-of context) destination value))

(defmethod pop-style ((context nuklear-context) (class (eql 'single-float)))
  (%nk:style-pop-float (handle-value-of context)))


(defmethod push-style ((context nuklear-context) destination (item style-item))
  (%nk:bge-style-push-style-item (handle-value-of context) destination (handle-value-of item)))

(defmethod pop-style ((context nuklear-context) (class (eql 'style-item)))
  (%nk:style-pop-style-item (handle-value-of context)))
