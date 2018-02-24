(cl:in-package :cl-bodge.graphics)


(declaim (special *active-framebuffer*
                  *active-renderbuffer*))


(defhandle framebuffer-handle
    :initform (gl:gen-framebuffer)
    :closeform (gl:delete-buffers (list *handle-value*)))


(defclass framebuffer (gl-object) ()
  (:default-initargs :handle (make-framebuffer-handle)))


(define-system-function make-framebuffer graphics-system ()
  (make-instance 'framebuffer))


(defmacro with-bound-framebuffer ((fbuf) &body body)
  `(unwind-protect
        (let ((*active-framebuffer* (handle-value-of ,fbuf)))
          (gl:bind-framebuffer :framebuffer *active-framebuffer*)
          ,@body)
     (gl:bind-framebuffer :framebuffer (bound-symbol-value *active-framebuffer* 0))))


(definline index->color-attachment (idx)
  (+ (cffi:foreign-enum-value '%gl:enum :color-attachment0) idx))


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
(defhandle renderbuffer-handle
    :initform (gl:gen-renderbuffer)
    :closeform (gl:delete-renderbuffers (list *handle-value*)))


(defclass renderbuffer (gl-object) ()
  (:default-initargs :handle (make-renderbuffer-handle)))


(defmacro with-bound-renderbuffer ((rbuf) &body body)
  `(unwind-protect
        (let ((*active-renderbuffer* (handle-value-of ,rbuf)))
          (gl:bind-renderbuffer :renderbuffer *active-renderbuffer*)
          ,@body)
     (gl:bind-renderbuffer :renderbuffer (bound-symbol-value *active-renderbuffer* 0))))


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


(defmethod attach-rendering-surface ((this framebuffer) (that renderbuffer) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-renderbuffer :framebuffer target :renderbuffer (handle-value-of that))))


(defmethod detach-rendering-surface ((this framebuffer) (that renderbuffer) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-renderbuffer :framebuffer target :renderbuffer 0)))
