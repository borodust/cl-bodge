(cl:in-package :cl-bodge.graphics)


(declaim (special *supplementary-framebuffer*
                  *depth-stencil-renderbuffer*))



(defgeneric %clear-rendering-output (output &key color))


(defmethod %clear-rendering-output ((this t) &key color)
  (gl:bind-framebuffer :framebuffer 0)
  (when color
    (gl:clear-color (x color) (y color) (z color) (w color)))
  (gl:clear :color-buffer :depth-buffer :stencil-buffer))


(defun clear-rendering-output (output &key color)
  (%clear-rendering-output output :color color))


(definline index->color-attachment (idx)
  (+ (cffi:foreign-enum-value '%gl:enum :color-attachment0) idx))


(defgeneric enable-rendering-output (output))


(defmethod enable-rendering-output ((output t))
  (gl:bind-framebuffer :framebuffer 0))


(defun bind-texture-framebuffer (texture)
  (gl:bind-framebuffer :framebuffer *supplementary-framebuffer*)
  (gl:framebuffer-texture-2d :framebuffer (index->color-attachment 0)
                             :texture-2d (%texture-id-of texture) 0)
  (destructuring-bind (width height) (texture-dimensions texture)
    (gl:bind-renderbuffer :renderbuffer *depth-stencil-renderbuffer*)
    (gl:renderbuffer-storage :renderbuffer :depth-stencil width height))
  (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                               :renderbuffer *depth-stencil-renderbuffer*))


(defmethod enable-rendering-output ((output texture-2d-input))
  (bind-texture-framebuffer output))


(defmethod %clear-rendering-output ((this texture-2d-input) &key color)
  (bind-texture-framebuffer this)
  (when color
    (gl:clear-color (x color) (y color) (z color) (w color)))
  (gl:clear :color-buffer :depth-buffer :stencil-buffer))

;;;
;;;
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
;;;
;;;
(defclass framebuffer (disposable)
  ((id :initform (gl:gen-framebuffer))))


(defun delete-framebuffer (id)
  (gl:delete-framebuffers (list id)))


(define-destructor framebuffer (id)
  (dispose-gl-object id #'delete-framebuffer))


(define-system-function make-framebuffer graphics-system ()
  (make-instance 'framebuffer :id (gl:gen-framebuffer)))


(defmethod enable-rendering-output ((output framebuffer))
  (%gl:bind-framebuffer :framebuffer (handle-value-of output)))


#|
(defgeneric attach-rendering-surface (framebuffer rendering-surface target))


(defgeneric detach-rendering-surface (framebuffer rendering-surface target))


(definline attach-color-buffer (framebuffer rendering-surface idx)
  (attach-rendering-surface framebuffer rendering-surface (index->color-attachment idx)))


(definline detach-color-buffer (framebuffer rendering-surface idx)
  (detach-rendering-surface framebuffer rendering-surface (index->color-attachment idx)))


(definline attach-depth-buffer (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :depth-attachment))


(definline detach-depth-buffer (framebuffer rendering-surface)
  (detach-rendering-surface framebuffer rendering-surface :depth-attachment))


(definline attach-stencil-buffer (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :stencil-attachment))


(definline detach-stencil-buffer (framebuffer rendering-surface)
  (detach-rendering-surface framebuffer rendering-surface :stencil-attachment))


(definline attach-depth-stencil-buffer (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :depth-stencil-attachment))


(definline detach-depth-stencil-buffer (framebuffer rendering-surface)
  (detach-rendering-surface framebuffer rendering-surface :depth-stencil-attachment))


(defmethod attach-rendering-surface ((this framebuffer) (that texture-2d) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-texture-2d :framebuffer target :texture-2d (handle-value-of that) 0)))


(defmethod detach-rendering-surface ((this framebuffer) (that texture-2d) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-texture-2d :framebuffer target :texture-2d 0 0)))


(defmacro with-complete-framebuffer ((fbuf &key color-buffers depth-buffer stencil-buffer
                                           depth-stencil-buffer) &body body)
  (once-only (fbuf)
    (multiple-value-bind (color-attach-list color-detach-list)
        (loop for cbuf in (ensure-list color-buffers)
           for idx = 0 then (1+ idx)
           collect `(attach-color-buffer ,fbuf ,cbuf ,idx) into attach
           collect `(detach-color-buffer ,fbuf ,cbuf ,idx) into detach
           finally (return (values attach detach)))
      (let ((attach-list (append color-attach-list
                                 (when depth-buffer
                                   (list `(attach-depth-buffer ,fbuf ,depth-buffer)))
                                 (when stencil-buffer
                                   (list `(attach-stencil-buffer ,fbuf ,depth-buffer)))
                                 (when depth-stencil-buffer
                                   (list `(attach-depth-stencil-buffer ,fbuf ,depth-buffer)))))
            (detach-list (append color-detach-list
                                 (when depth-buffer
                                   (list `(detach-depth-buffer ,fbuf ,depth-buffer)))
                                 (when stencil-buffer
                                   (list `(detach-stencil-buffer ,fbuf ,depth-buffer)))
                                 (when depth-stencil-buffer
                                   (list `(detach-depth-stencil-buffer ,fbuf ,depth-buffer))))))
        (when (and (null attach-list) (null detach-list))
          (error "At least one attachment must be specified for a framebuffer"))
        `(with-bound-framebuffer (,fbuf)
           (unwind-protect
                (progn
                  ,@attach-list
                  ,@body)
             ,@detach-list))))))



;;;
;;;
;;;



(defmethod attach-rendering-surface ((this framebuffer) (that renderbuffer) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-renderbuffer :framebuffer target :renderbuffer (handle-value-of that))))


(defmethod detach-rendering-surface ((this framebuffer) (that renderbuffer) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-renderbuffer :framebuffer target :renderbuffer 0)))
|#
