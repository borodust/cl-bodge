(cl:in-package :cl-bodge.graphics)


;;;
;;; FRAMEBUFFER ATTACHMENTS
;;;
(defgeneric framebuffer-attachment-buffer-type (attachment))
(defgeneric framebuffer-attachment-dimensions (attachment)
  (:method (attachment) (declare (ignore attachment)) nil))
(defgeneric bind-framebuffer-attachment (attachment))
(defgeneric unbind-framebuffer-attachment (attachment))


(defmacro with-color-attachments ((&optional (start 0)) &body body)
  `(let ((*current-color-attachment-index* ,start))
     ,@body))


(defun color-attachment-counter ()
  *current-color-attachment-index*)


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


(defmethod framebuffer-attachment-dimensions ((this texture-input))
  (texture-dimensions this))


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


(defmethod framebuffer-attachment-dimensions ((this texture-2d))
  (texture-dimensions this))


(defmethod bind-framebuffer-attachment ((this depth-texture))
  (%gl:framebuffer-texture :framebuffer :depth-attachment (%texture-id-of this) 0))


(defmethod unbind-framebuffer-attachment ((this depth-texture))
  (%gl:framebuffer-texture :framebuffer :depth-attachment 0 0))


(defmethod framebuffer-attachment-buffer-type ((this cubemap-texture-layer))
  :color)


(defmethod framebuffer-attachment-dimensions ((this cubemap-texture-layer))
  (texture-dimensions (%texture-of this)))


(defmethod bind-framebuffer-attachment ((this cubemap-texture-layer))
  (gl:framebuffer-texture-2d :framebuffer (next-color-attachment-index)
                             (%layer-type-of this) (%texture-id-of (%texture-of this)) 0))


(defmethod unbind-framebuffer-attachment ((this cubemap-texture-layer))
  (gl:framebuffer-texture-2d :framebuffer (prev-color-attachment-index)
                             (%layer-type-of this) 0 0))
