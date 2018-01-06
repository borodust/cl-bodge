(in-package :cl-bodge.ui)


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
   (windows :initform nil :reader %windows-of)
   (font :initarg :font :reader font-of)))


(define-destructor nuklear-context (nuklear-font canvas)
  (dispose nuklear-font)
  (dispose canvas))


(defun calc-text-width (text)
  (with-font ((font-of *context*) (canvas-of *context*))
    (multiple-value-bind (bounds advance) (canvas-text-bounds text (canvas-of *context*))
      (declare (ignore bounds))
      advance)))


(bodge-nuklear:define-text-width-callback calc-string-width (handle height string)
  (calc-text-width string))


(defun ui-font (name size canvas-font-container
                &key letter-spacing line-height alignment)
  (list name canvas-font-container size letter-spacing line-height alignment))


(defmethod initialize-instance ((this nuklear-context) &rest keys &key width height
                                                                    font-descriptor
                                                                    antialiased
                                                                    pixel-ratio)
  (let ((canvas (make-canvas width height :antialiased antialiased
                                          :pixel-ratio pixel-ratio)))
    (destructuring-bind (name canvas-font-container size letter-spacing line-height alignment)
        font-descriptor
      (let* ((font-face-id (register-font-face name canvas-font-container canvas))
             (font (make-font font-face-id size
                              :letter-spacing letter-spacing
                              :line-height line-height
                              :alignment alignment))
             (nk-font (make-nuklear-font (canvas-font-line-height (canvas-font-metrics font canvas))
                                         'calc-string-width)))
        (apply #'call-next-method this
               :handle (make-nuklear-context-handle
                        (bodge-nuklear:make-context (handle-value-of nk-font)))
               :canvas canvas
               :nuklear-font nk-font
               :font font
               keys)))))


(definline make-ui-context (width height font-descriptor
                                  &key antialiased (pixel-ratio 1.0))
  (make-instance 'nuklear-context
                 :width width
                 :height height
                 :pixel-ratio pixel-ratio
                 :font-descriptor font-descriptor
                 :antialiased antialiased))


(defun add-window (context window)
  (with-slots (windows) context
    (push window windows)))


(defun remove-window (context window)
  (with-slots (windows) context
    (deletef windows window)))


(defun push-compose-task (ctx fn)
  (with-slots (compose-tasks) ctx
    (push-task fn compose-tasks)))


(defmacro with-ui-access ((ctx) &body body)
  `(push-compose-task ,ctx (lambda () ,@body)))


(defun drain-compose-task-queue (ctx)
  (with-slots (compose-tasks) ctx
    (drain compose-tasks)))


(defmacro with-ui ((ctx) &body body)
  `(let ((*context* ,ctx)
         (*handle* (handle-value-of ,ctx)))
     ,@body))


(defmacro with-ui-input ((ui) &body body)
  `(with-ui (,ui)
     (prog2
         (%nk:input-begin *handle*)
         (progn ,@body)
       (%nk:input-end *handle*))))


(definline clear-ui-context (&optional (ui *context*))
  (%nk:clear (handle-value-of ui)))


(defun register-cursor-position (x y)
  (%nk:input-motion *handle* (floor x) (floor (- (height-of *context*) y))))


(defun register-character-input (character)
  (%nk:input-unicode *handle* (char-code character)))


(defun update-ui-size (ui width height)
  (with-slots ((w width) (h height) text-renderer canvas) ui
    (update-canvas-size canvas width height)
    (setf w width
          h height)))


(defun button-state->nk (state)
  (ecase state
    (:pressed 1)
    (:released 0)))


(defun key->nk (key)
  (ecase key
    (:backspace %nk:+key-backspace+)
    (:left %nk:+key-left+)
    (:right %nk:+key-right+)))


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
