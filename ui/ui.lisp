(cl:in-package :cl-bodge.ui)


(defclass ui (disposable)
  ((renderer :initarg :renderer :reader %renderer-of)
   (handle :initarg :handle :reader %handle-of)))


(define-destructor ui (renderer handle)
  (dispose handle)
  (run (for-graphics ()
         (dispose renderer))))


(define-system-function make-ui graphics-system (width height &key (pixel-ratio 1f0)
                                                       input-source
                                                       (antialiased t))
  (let ((renderer (make-ui-canvas-renderer width height :antialiased antialiased
                                                        :pixel-ratio pixel-ratio)))
    (make-instance 'ui :handle (bodge-ui:make-ui renderer :input-source input-source)
                       :renderer renderer)))


(defun compose-ui (ui)
  (bodge-ui:compose-ui (%handle-of ui)))


(defmacro with-ui-access ((ui) &body body)
  `(bodge-ui:with-ui-access ((%handle-of ,ui))
     ,@body))


(defun add-window (ui window-class &rest initargs &key &allow-other-keys)
  (apply #'bodge-ui:add-window (%handle-of ui) window-class initargs))


(defun remove-window (ui window)
  (bodge-ui:remove-window (%handle-of ui) window))


(defun remove-all-windows (ui)
  (bodge-ui:remove-all-windows (%handle-of ui)))


(defun update-ui-size (ui width height)
  (update-renderer-canvas-size (%renderer-of ui) width height))


(defun update-ui-pixel-ratio (ui pixel-ratio)
  (update-renderer-canvas-pixel-ratio (%renderer-of ui) pixel-ratio))
