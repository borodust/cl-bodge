(in-package :cl-bodge.canvas)

(declaim (special *canvas*))


(defhandle canvas-handle
    :initform (bodge-nanovg:make-context :stencil-strokes (when-debugging :debug))
    :closeform (bodge-nanovg:destroy-context *handle-value*))


(defclass canvas (foreign-object) ()
  (:default-initargs :handle (make-canvas-handle)))


(define-system-function make-canvas graphics-system ()
  (make-instance 'canvas))


(defmacro with-canvas ((canvas width height &optional (pixel-ratio 1.0)) &body body)
  `(preserving-state
     (let ((*canvas* (handle-value-of ,canvas)))
       (unwind-protect
            (progn
              (%nvg:begin-frame *canvas* ,width ,height ,pixel-ratio)
              ,@body)
         (%nvg:end-frame *canvas*)))))


(defmacro path (&body body)
  `(unwind-protect
        (progn
          (%nvg:begin-path *canvas*)
          ,@body)
     (%nvg:fill *canvas*)))


(defun draw-rect (x y w h)
  (%nvg:rect *canvas* x y w h))
