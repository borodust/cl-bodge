(in-package :cl-bodge.graphics)


(defhandle framebuffer-handle
    :initform (gl:gen-framebuffer)
    :closeform (gl:delete-buffers (list *handle-value*)))


(defclass framebuffer (gl-object) ()
  (:default-initargs :handle (make-framebuffer-handle)))
