(cl:in-package :cl-bodge.graphics)


(declaim (special *supplementary-framebuffer*
                  *depth-stencil-renderbuffer*))



(defvar *default-clear-color* (vec4 1 1 1 1))

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
(defmethod %clear-rendering-output ((this texture-2d-input) color)
  (flet ((%clear ()
           (gl:clear-color (x color) (y color) (z color) (w color))
           (gl:clear :color-buffer :depth-buffer :stencil-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod run-with-bound-output ((texture texture-2d-input) function)
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
;;; FRAMEBUFFER
;;;
(defclass framebuffer (disposable)
  ((id :initform (gl:gen-framebuffer))))


(defun delete-framebuffer (id)
  (gl:delete-framebuffers (list id)))


(define-destructor framebuffer (id)
  (dispose-gl-object id #'delete-framebuffer))


(define-system-function make-framebuffer graphics-system ()
  (make-instance 'framebuffer :id (gl:gen-framebuffer)))


(defmethod run-with-bound-output ((output framebuffer) function)
  (%gl:bind-framebuffer :framebuffer (handle-value-of output))
  (funcall function))
