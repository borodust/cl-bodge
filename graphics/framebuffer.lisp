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
