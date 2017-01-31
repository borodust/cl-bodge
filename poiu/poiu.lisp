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
   (framebuffer :initform nil :reader framebuffer-of)
   (compose-tasks :initform (make-task-queue))
   (depth-stencil-buffer :initform nil)
   (overlay-texture :initform nil :reader overlay-of)
   (text-renderer :initarg :text-renderer :reader text-renderer-of)))


(defmethod font-of ((ctx nuklear-context))
  (with-slots (text-renderer) ctx
    (font-of text-renderer)))


(define-destructor nuklear-context (nuklear-font canvas framebuffer depth-stencil-buffer
                                                 overlay-texture)
  (dispose nuklear-font)
  (dispose canvas)
  (dispose framebuffer)
  (dispose depth-stencil-buffer)
  (dispose overlay-texture))


(bodge-nuklear:define-font-width-callback calc-string-width (handle height string)
  (let ((w (first (measure-string string (font-of *context*)))))
    (* w (scale-of (text-renderer-of *context*)))))


(defmethod initialize-instance ((this nuklear-context) &rest keys &key width height
                                                                    font line-height
                                                                    antialiased-p)
  (let ((nk-font (make-nuklear-font line-height 'calc-string-width)))
    (apply #'call-next-method this
           :handle (make-nuklear-context-handle
                    (bodge-nuklear:make-context (handle-value-of nk-font)))
           :canvas (make-canvas :antialiased-p antialiased-p)
           :nuklear-font nk-font
           :text-renderer (make-text-renderer width height font line-height)
           keys)))


(defmethod initialize-instance :after ((this nuklear-context) &key)
  (with-slots (width height filter-texture framebuffer depth-stencil-buffer overlay-texture)
      this
    (setf overlay-texture (make-2d-texture (make-blank-image width height) :grey
                                           :generate-mipmaps-p nil)
          framebuffer (make-framebuffer)
          depth-stencil-buffer (make-renderbuffer :depth-stencil width height))
    (attach-depth-stencil-buffer framebuffer depth-stencil-buffer)))


(definline make-poiu-context (width height font line-height &key antialiased-p)
  (make-instance 'nuklear-context
                 :width width
                 :height height
                 :font font
                 :line-height line-height
                 :antialiased-p antialiased-p))


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
