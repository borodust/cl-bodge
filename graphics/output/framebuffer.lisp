(cl:in-package :cl-bodge.graphics)



;;;
;;; FRAMEBUFFER
;;;
(defclass framebuffer (disposable)
  ((id :initarg :id :initform (gl:gen-framebuffer))
   (attachments :initform nil)
   (color-attachment-count :initform 0)
   (width :initform 0)
   (height :initform 0)
   (bound-buffers :initform nil)))


(defun delete-framebuffer (id)
  (gl:delete-framebuffers (list id)))


(define-destructor framebuffer (id)
  (dispose-gl-object id #'delete-framebuffer))


(define-system-function make-framebuffer graphics-system ()
  (make-instance 'framebuffer :id (gl:gen-framebuffer)))


(defun configure-framebuffer (framebuffer &rest attachments)
  (with-slots ((this-attachments attachments) bound-buffers id color-attachment-count
               (this-width width) (this-height height))
      framebuffer
    (%gl:bind-framebuffer :framebuffer id)
    (with-color-attachments (color-attachment-count)
      (dolist (attachment this-attachments)
        (unbind-framebuffer-attachment attachment)))
    (setf this-attachments (reverse attachments)
          bound-buffers nil)
    (unwind-protect
         (let (width height)
           (with-color-attachments ()
             (dolist (attachment attachments)
               (ecase (framebuffer-attachment-buffer-type attachment)
                 (:color (pushnew :color-buffer bound-buffers))
                 (:depth (pushnew :depth-buffer bound-buffers))
                 (:stencil (pushnew :stencil-buffer bound-buffers))
                 (:depth-stencil
                  (pushnew :depth-buffer bound-buffers)
                  (pushnew :stencil-buffer bound-buffers)))
               (destructuring-bind (&optional attachment-width attachment-height)
                   (framebuffer-attachment-dimensions attachment)
                 (when (or (null width) (and attachment-width (< attachment-width width)))
                   (setf width attachment-width))
                 (when (or (null height) (and attachment-height (< attachment-height height)))
                   (setf height attachment-height)))
               (bind-framebuffer-attachment attachment))
             (setf color-attachment-count (color-attachment-counter)))
           (setf this-width (floor (or width (viewport-width)))
                 this-height (floor (or height (viewport-height)))))
      (%gl:bind-framebuffer :framebuffer *active-framebuffer*))))


(defmethod %clear-rendering-output ((this framebuffer) color)
  (with-slots (bound-buffers) this
    (flet ((%clear ()
             (gl:clear-color (x color) (y color) (z color) (w color))
             (apply #'gl:clear bound-buffers)))
      (run-with-bound-output this #'%clear))))


(defmethod run-with-bound-output ((output framebuffer) function)
  (with-slots (attachments id width height) output
    (let ((*active-framebuffer* id))
      (%gl:bind-framebuffer :framebuffer id)
      (gl:viewport 0 0 width height)
      (unwind-protect
           (funcall function)
        (%gl:bind-framebuffer :framebuffer *active-framebuffer*)))))
