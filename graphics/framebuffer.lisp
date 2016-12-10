(in-package :cl-bodge.graphics)


(declaim (special *active-framebuffer*))


(defhandle framebuffer-handle
    :initform (gl:gen-framebuffer)
    :closeform (gl:delete-buffers (list *handle-value*)))


(defclass framebuffer (gl-object) ()
  (:default-initargs :handle (make-framebuffer-handle)))


(defmacro with-bound-framebuffer ((fbuf) &body body)
  `(let ((*active-framebuffer* (handle-value-of ,fbuf)))
     (unwind-protect
          (progn
            (gl:bind-framebuffer :framebuffer *active-framebuffer*)
            ,@body)
       (gl:bind-framebuffer (bound-symbol-value *active-framebuffer* 0)))))


(definline index->color-attachment (idx)
  (+ (cffi:foreign-enum-value '%gl:enum :color-attachment0) idx))


(defgeneric attach-rendering-surface (framebuffer rendering-surface target))


(definline attach-color-target (framebuffer rendering-surface idx)
  (attach-rendering-surface framebuffer rendering-surface (index->color-attachment idx)))


(definline attach-depth-target (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :depth-attachment))


(definline attach-stencil-target (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :stencil-attachment))


(definline attach-depth-stencil-target (framebuffer rendering-surface)
  (attach-rendering-surface framebuffer rendering-surface :depth-stencil-attachment))


(defmethod attach-rendering-surface ((this framebuffer) (that texture-2d) target)
  (with-bound-framebuffer (this)
    (gl:framebuffer-texture-2d :framebuffer target :texture-2d (handle-value-of that) 0)))
