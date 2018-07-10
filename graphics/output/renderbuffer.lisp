(cl:in-package :cl-bodge.graphics)



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
