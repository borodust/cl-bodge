(in-package :cl-bodge.canvas)

(declaim (special *canvas*))


(defhandle canvas-handle
    :closeform (bodge-nanovg:destroy-context *handle-value*))


(defclass canvas (foreign-object) ())


(define-system-function make-canvas graphics-system (&key antialiased-p)
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased-p (list :antialias))
                      (when-debugging (list :debug)))))
    (make-instance 'canvas
                   :handle (make-canvas-handle (apply #'bodge-nanovg:make-context opts)))))


(definline begin-canvas (canvas width height &optional (pixel-ratio 1.0))
  (%nvg:begin-frame (handle-value-of canvas) width height pixel-ratio))


(definline end-canvas (canvas)
  (%nvg:end-frame (handle-value-of canvas)))


(defmacro with-canvas ((canvas width height &optional (pixel-ratio 1.0)) &body body)
  (once-only (canvas)
    `(preserving-state
       (let ((*canvas* (handle-value-of ,canvas)))
         (unwind-protect
              (progn
                (begin-canvas ,canvas ,width ,height ,pixel-ratio)
                ,@body)
           (end-canvas ,canvas))))))


(definline fill-path (&optional (canvas *canvas*))
  (%nvg:fill (handle-value-of canvas)))


(definline stroke-path (&optional (canvas *canvas*))
  (%nvg:stroke (handle-value-of canvas)))


(definline move-to (coords &optional (canvas *canvas*))
  (%nvg:move-to (handle-value-of canvas) (x coords) (y coords)))


(defun stroke-color (color &optional (canvas *canvas*))
  (c-with ((nk-color (:struct (%nk:color))))
    (%nvg:rgba-f nk-color (x color) (y color) (z color) (w color))
    (%nvg:stroke-color (handle-value-of canvas) nk-color)))


(definline stroke-width (width &optional (canvas *canvas*))
  (%nvg:stroke-width (handle-value-of canvas) width))


(defun fill-color (color &optional (canvas *canvas*))
  (c-with ((nk-color (:struct (%nk:color))))
    (%nvg:rgba-f nk-color (x color) (y color) (z color) (w color))
    (%nvg:fill-color (handle-value-of canvas) nk-color)))


(defmacro path ((&optional (canvas *canvas*)) &body body)
  `(progn
     (%nvg:begin-path (handle-value-of ,canvas))
     ,@body))


(defun scissors (origin w h &optional (canvas *canvas*))
  (path (canvas)
    (%nvg:scissor (handle-value-of canvas) (x origin) (y origin) #f w #f h)))


(definline stroke-and-fill (stroke-color fill-color thickness canvas)
  (when fill-color
    (fill-color fill-color canvas)
    (fill-path canvas))
  (when stroke-color
    (stroke-width thickness canvas)
    (stroke-color stroke-color canvas)
    (stroke-path canvas)))


(defun draw-line (origin end color &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to origin canvas)
    (%nvg:line-to (handle-value-of canvas) (x end) (y end))
    (stroke-width thickness canvas)
    (stroke-color color canvas)
    (stroke-path canvas)))


(defun draw-curve (origin end ctrl0 ctrl1 color &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to origin canvas)
    (%nvg:bezier-to (handle-value-of canvas) (x ctrl0) (y ctrl0) (x ctrl1) (y ctrl1)
                    (x end) (y end))
    (stroke-color color canvas)
    (stroke-width thickness canvas)
    (stroke-path canvas)))


(defun draw-rect (origin w h &key (fill-color nil) (stroke-color nil)
                               (thickness 1.0) (rounding 0.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:rounded-rect (handle-value-of canvas) (x origin) (y origin) w h rounding)
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-circle (center radius &key (fill-color nil) (stroke-color nil)
                                    (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:circle (handle-value-of canvas) (x center) (y center) radius)
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-ellipse (center x-radius y-radius &key (fill-color nil) (stroke-color nil)
                                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:ellipse (handle-value-of canvas) (x center) (y center) x-radius y-radius)
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-arc (center radius a0 a1 &key (fill-color nil) (stroke-color nil)
                                       (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:arc (handle-value-of canvas) (x center) (y center) radius a0 a1 %nvg:+ccw+)
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-polygon (vertices &key (fill-color nil) (stroke-color nil)
                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (let ((first-vertex (car vertices)))
      (move-to first-vertex canvas)
      (loop for vertex in (rest vertices)
         do (%nvg:line-to (handle-value-of canvas) (x vertex) (y vertex)))
      (%nvg:line-to (handle-value-of canvas) (x first-vertex) (y first-vertex)))
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-polyline (points color &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to (car points) canvas)
    (loop for point in (rest points)
       do (%nvg:line-to (handle-value-of canvas) (x point) (y point)))
    (stroke-color color)
    (stroke-width thickness)
    (stroke-path)))
