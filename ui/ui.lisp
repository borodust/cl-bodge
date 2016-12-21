(in-package :cl-bodge.interactions)


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


(defclass nuklear-context (foreign-object)
  ((nuklear-font :initarg :nuklear-font)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (canvas :initarg :canvas :reader canvas-of)
   (text-renderer :initarg :text-renderer :reader text-renderer-of)))


(defun font-of (ctx)
  (with-slots (text-renderer) ctx
    (text-renderer-font text-renderer)))


(define-destructor nuklear-context (nuklear-font canvas)
  (dispose nuklear-font)
  (dispose canvas))


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


(definline make-poiu-context (width height font line-height &key antialiased-p)
  (make-instance 'nuklear-context
                 :width width
                 :height height
                 :font font
                 :line-height line-height
                 :antialiased-p antialiased-p))


(defmacro with-poiu ((ctx) &body body)
  `(let ((*context* ,ctx)
         (*handle* (handle-value-of ,ctx)))
     ,@body))


(defmacro layout-row ((height columns) &body body)
  `(prog2
       (%nk:layout-row-begin *handle* %nk:+static+ ,height ,columns)
       (progn ,@body)
     (%nk:layout-row-end *handle*)))


(defmacro in-window ((x y w h &key (title "")) &body body)
  `(unwind-protect
        (progn
          (%nk:begin *handle* ,title (%nk:rect ,x ,y ,w ,h) 0)
          ,@body)
     (%nk:end *handle*)))


(defmacro with-poiu-input (&body body)
  `(prog2
       (%nk:input-begin *handle*)
       (progn ,@body)
     (%nk:input-end *handle*)))


(definline clear-poiu-context ()
  (%nk:clear *handle*))
