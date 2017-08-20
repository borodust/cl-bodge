(in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(defhandle canvas-handle
    :closeform (bodge-nanovg:destroy-context *handle-value*))


(defclass canvas (foreign-object)
  ((width :initarg :width :initform (error ":width missing") :reader width-of)
   (height :initarg :height :initform (error ":height missing") :reader height-of)))


(defun update-canvas-size (canvas width height)
  (with-slots ((w width) (h height)) canvas
    (setf w width
          h height)))


(define-system-function make-canvas graphics-system (width height &key antialiased)
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased (list :antialias))
                      (in-development-mode (list :debug)))))
    (make-instance 'canvas
                   :handle (make-canvas-handle (apply #'bodge-nanovg:make-context opts))
                   :width width
                   :height height)))


(definline begin-canvas (canvas &optional (pixel-ratio 1.0))
  (%nvg:begin-frame (handle-value-of canvas) (width-of canvas) (height-of canvas) pixel-ratio))


(definline end-canvas (canvas)
  (%nvg:end-frame (handle-value-of canvas))
  (reset-state))


(defmacro with-canvas ((canvas &optional (pixel-ratio 1.0)) &body body)
  (once-only (canvas)
    `(let ((*canvas* ,canvas))
       (unwind-protect
            (progn
              (begin-canvas ,canvas ,pixel-ratio)
              ,@body)
         (end-canvas ,canvas)))))



(definline fill-path (&optional (canvas *canvas*))
  (%nvg:fill (handle-value-of canvas)))


(definline stroke-path (&optional (canvas *canvas*))
  (%nvg:stroke (handle-value-of canvas)))


(definline move-to (coords &optional (canvas *canvas*))
  (%nvg:move-to (handle-value-of canvas) (x coords) (%invert (y coords) canvas)))


(definline (setf stroke-width) (width &optional (canvas *canvas*))
  (%nvg:stroke-width (handle-value-of canvas) (f width)))


(defmacro path ((&optional (canvas *canvas*)) &body body)
  `(progn
     (%nvg:begin-path (handle-value-of ,canvas))
     ,@body))


(defun stroke-color (color &optional (canvas *canvas*))
  (%nvg:bge-stroke-color (handle-value-of canvas) (x color) (y color) (z color) (w color)))


(defun fill-color (color &optional (canvas *canvas*))
  (%nvg:bge-fill-color (handle-value-of canvas) (x color) (y color) (z color) (w color)))


(definline %invert (y canvas &optional (h 0.0))
  (- (height-of canvas) y h))


(defun push-canvas (&key (canvas *canvas*))
  (%nvg:save (handle-value-of canvas)))


(defun pop-canvas (&key (canvas *canvas*))
  (%nvg:restore (handle-value-of canvas)))


(defun reset-canvas (&key (canvas *canvas*))
  (%nvg:reset (handle-value-of canvas)))
