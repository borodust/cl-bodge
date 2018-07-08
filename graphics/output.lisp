(cl:in-package :cl-bodge.graphics)


(declaim (special *supplementary-framebuffer*
                  *depth-stencil-renderbuffer*
                  *current-color-attachment-index*))


(defvar *default-clear-color* (vec4 1 1 1 1))
(defvar *active-framebuffer* 0)

(defgeneric run-with-bound-output (output function))
(defgeneric %clear-rendering-output (output color))

(defun clear-rendering-output (output &key color)
  (%clear-rendering-output output (or color *default-clear-color*)))


(definline index->color-attachment (idx)
  (+ (cffi:foreign-enum-value '%gl:enum :color-attachment0) idx))


;;;
;;; DEFAULT FRAMEBUFFER
;;;
(defmethod %clear-rendering-output ((this t) color)
  (gl:bind-framebuffer :framebuffer 0)
  (gl:clear-color (x color) (y color) (z color) (w color))
  (gl:clear :color-buffer :stencil-buffer :depth-buffer))


(defmethod run-with-bound-output ((output t) function)
  (gl:bind-framebuffer :framebuffer 0)
  (funcall function))


;;;
;;; TEXTURES
;;;
(defmethod %clear-rendering-output ((this texture-2d) color)
  (flet ((%clear ()
           (gl:clear-color (x color) (y color) (z color) (w color))
           (gl:clear :color-buffer :depth-buffer :stencil-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod run-with-bound-output ((texture texture-2d) function)
  (gl:bind-framebuffer :framebuffer *supplementary-framebuffer*)
  (gl:draw-buffer :color-attachment0)
  (gl:framebuffer-texture-2d :framebuffer (index->color-attachment 0)
                             :texture-2d (%texture-id-of texture) 0)
  (destructuring-bind (width height) (texture-dimensions texture)
    (gl:bind-renderbuffer :renderbuffer *depth-stencil-renderbuffer*)
    (gl:renderbuffer-storage :renderbuffer :depth-stencil width height))
  (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                               :renderbuffer *depth-stencil-renderbuffer*)
  (unwind-protect
       (funcall function)
    (gl:framebuffer-texture-2d :framebuffer (index->color-attachment 0)
                               :texture-2d 0 0)
    (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                                 :renderbuffer 0)))


(defmethod %clear-rendering-output ((this depth-texture) color)
  (flet ((%clear ()
           (gl:clear :depth-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod run-with-bound-output ((texture depth-texture) function)
  (gl:bind-framebuffer :framebuffer *supplementary-framebuffer*)
  (gl:draw-buffer :none)
  (%gl:framebuffer-texture :framebuffer :depth-attachment (%texture-id-of texture) 0)
  (unwind-protect
       (funcall function)
    (%gl:framebuffer-texture :framebuffer :depth-attachment 0 0)))


;;;
;;; RENDERBUFFER
;;;
(defclass renderbuffer (disposable)
  ((id :initform (gl:gen-renderbuffer) :reader %id-of)))


(defun delete-renderbuffer (id)
  (gl:delete-renderbuffers (list id)))


(define-destructor renderbuffer (id)
  (dispose-gl-object id #'delete-renderbuffer))


(defmacro with-bound-renderbuffer ((rbuf) &body body)
  (with-gensyms (active-renderbuffer)
    `(let ((,active-renderbuffer (%id-of ,rbuf)))
       (unwind-protect
            (progn
              (gl:bind-renderbuffer :renderbuffer ,active-renderbuffer)
              ,@body)
         (gl:bind-renderbuffer :renderbuffer 0)))))


(defmethod initialize-instance :after ((this renderbuffer) &key
                                                             texture-format
                                                             width
                                                             height
                                                             (samples 0))
  (with-bound-renderbuffer (this)
    (gl:renderbuffer-storage-multisample :renderbuffer samples
                                         (%texture-format->internal-format texture-format)
                                         width height)))


(define-system-function make-renderbuffer graphics-system (texture-format width height
                                                                          &optional (samples 0))
  (make-instance 'renderbuffer
                 :texture-format texture-format
                 :width width
                 :height height
                 :samples samples))


;;;
;;; FRAMEBUFFER ATTACHMENTS
;;;
(defgeneric framebuffer-attachment-buffer-type (attachment))
(defgeneric bind-framebuffer-attachment (attachment))
(defgeneric unbind-framebuffer-attachment (attachment))


(defmacro with-color-attachments (() &body body)
  `(let ((*current-color-attachment-index* 0))
     ,@body))


(defun next-color-attachment-index ()
  (prog1 (index->color-attachment *current-color-attachment-index*)
    (incf *current-color-attachment-index*)))


(defun prev-color-attachment-index ()
  (index->color-attachment (decf *current-color-attachment-index*)))


(defclass color-attachment-placeholder () ())


(defmethod framebuffer-attachment-buffer-type ((this color-attachment-placeholder))
  :color)


(defmethod bind-framebuffer-attachment (attachment)
  (next-color-attachment-index))


(defmethod unbind-framebuffer-attachment (attachment)
  (prev-color-attachment-index))


(defun color-attachment-placeholder ()
  (make-instance 'color-attachment-placeholder))


(defmethod framebuffer-attachment-buffer-type ((this texture-2d))
  :color)


(defmethod bind-framebuffer-attachment ((this texture-2d))
  (gl:framebuffer-texture-2d :framebuffer (next-color-attachment-index)
                             :texture-2d (%texture-id-of this) 0))


(defmethod unbind-framebuffer-attachment ((this texture-2d))
  (gl:framebuffer-texture-2d :framebuffer (prev-color-attachment-index)
                             :texture-2d 0))


(defmethod framebuffer-attachment-buffer-type ((this depth-texture))
  :depth)


(defmethod bind-framebuffer-attachment ((this depth-texture))
  (%gl:framebuffer-texture :framebuffer :depth-attachment (%texture-id-of this) 0))


(defmethod unbind-framebuffer-attachment ((this depth-texture))
  (%gl:framebuffer-texture :framebuffer :depth-attachment 0 0))


;;;
;;; FRAMEBUFFER
;;;
(defclass framebuffer (disposable)
  ((id :initarg :id :initform (gl:gen-framebuffer))
   (attachments :initform nil)
   (bound-buffers :initform nil)))


(defun delete-framebuffer (id)
  (gl:delete-framebuffers (list id)))


(define-destructor framebuffer (id)
  (dispose-gl-object id #'delete-framebuffer))


(define-system-function make-framebuffer graphics-system ()
  (make-instance 'framebuffer :id (gl:gen-framebuffer)))


(defun configure-framebuffer (framebuffer &rest attachments)
  (with-slots ((this-attachments attachments) bound-buffers id) framebuffer
    (dolist (attachment attachments)
      (ecase (framebuffer-attachment-buffer-type attachment)
        (:color (pushnew :color-buffer bound-buffers))
        (:depth (pushnew :depth-buffer bound-buffers))
        (:stencil (pushnew :stencil-buffer bound-buffers))
        (:depth-stencil (pushnew :depth-buffer bound-buffers)
                        (pushnew :stencil-buffer bound-buffers))))
    (%gl:bind-framebuffer :framebuffer id)
    (unwind-protect
         (with-color-attachments ()
           (dolist (attachment attachments)
             (bind-framebuffer-attachment attachment)))
      (%gl:bind-framebuffer :framebuffer *active-framebuffer*))))


(defmethod %clear-rendering-output ((this framebuffer) color)
  (with-slots (bound-buffers) this
    (flet ((%clear ()
             (gl:clear-color (x color) (y color) (z color) (w color))
             (apply #'gl:clear bound-buffers)))
      (run-with-bound-output this #'%clear))))


(defmethod run-with-bound-output ((output framebuffer) function)
  (with-slots (attachments id) output
    (let ((*active-framebuffer* id))
      (%gl:bind-framebuffer :framebuffer id)
      (unwind-protect
           (funcall function)
        (%gl:bind-framebuffer :framebuffer *active-framebuffer*)))))
