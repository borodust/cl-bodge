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


(defmacro with-canvas ((canvas width height &optional (pixel-ratio 1.0)) &body body)
  `(preserving-state
     (let ((*canvas* (handle-value-of ,canvas)))
       (unwind-protect
            (progn
              (%nvg:begin-frame *canvas* ,width ,height ,pixel-ratio)
              ,@body)
         (%nvg:end-frame *canvas*)))))


(definline fill-path ()
  (%nvg:fill *canvas*))


(definline stroke-path ()
  (%nvg:stroke *canvas*))


(definline move-to (coords)
  (%nvg:move-to *canvas* (x coords) (y coords)))


(defun stroke-color (color)
  (c-with ((nk-color (:struct (%nk:color))))
    (%nvg:rgba-f nk-color (x color) (y color) (z color) (w color))
    (%nvg:stroke-color *canvas* nk-color)))


(definline stroke-width (width)
  (%nvg:stroke-width *canvas* width))


(defun fill-color (color)
  (c-with ((nk-color (:struct (%nk:color))))
    (%nvg:rgba-f nk-color (x color) (y color) (z color) (w color))
    (%nvg:fill-color *canvas* nk-color)))


(defmacro path (&body body)
  `(progn
     (%nvg:begin-path *canvas*)
     ,@body))


(defun scissors (origin w h)
  (path
    (%nvg:scissor *canvas* (x origin) (y origin) #f w #f h)))


(definline stroke-and-fill (stroke-color fill-color thickness)
  (when fill-color
    (fill-color fill-color)
    (fill-path))
  (when stroke-color
    (stroke-width thickness)
    (stroke-color stroke-color)
    (stroke-path)))


(defun draw-line (origin end color &key (thickness 1.0))
  (path
    (move-to origin)
    (%nvg:line-to *canvas* (x end) (y end))
    (stroke-width thickness)
    (stroke-color color)
    (stroke-path)))


(defun draw-curve (origin end ctrl0 ctrl1 color &key (thickness 1.0))
  (path
    (%nvg:move-to *canvas* (x origin) (y origin))
    (%nvg:bezier-to *canvas* (x ctrl0) (y ctrl0) (x ctrl1) (y ctrl1) (x end) (y end))
    (stroke-color color)
    (stroke-width thickness)
    (stroke-path)))


(defun draw-rect (origin w h &key (fill-color nil) (stroke-color nil)
                               (thickness 1.0) (rounding 0.0))
  (path
    (%nvg:rounded-rect *canvas* (x origin) (y origin) w h rounding)
    (stroke-and-fill stroke-color fill-color thickness)))


(defun draw-circle (center radius &key (fill-color nil) (stroke-color nil)
                                    (thickness 1.0))
  (path
    (%nvg:circle *canvas* (x center) (y center) radius)
    (stroke-and-fill stroke-color fill-color thickness)))


(defun draw-ellipse (center x-radius y-radius &key (fill-color nil) (stroke-color nil)
                                                (thickness 1.0))
  (path
    (%nvg:ellipse *canvas* (x center) (y center) x-radius y-radius)
    (stroke-and-fill stroke-color fill-color thickness)))


(defun draw-arc (center radius a0 a1 &key (fill-color nil) (stroke-color nil)
                                       (thickness 1.0))
  (path
    (%nvg:arc *canvas* (x center) (y center) radius a0 a1 %nvg:+ccw+)
    (stroke-and-fill stroke-color fill-color thickness)))


(defun draw-polygon (vertices &key (fill-color nil) (stroke-color nil)
                                (thickness 1.0))
  (path
    (let ((first-vertex (car vertices)))
      (move-to first-vertex)
      (loop for vertex in (rest vertices)
         do (%nvg:line-to *canvas* (x vertex) (y vertex)))
      (%nvg:line-to *canvas* (x first-vertex) (y first-vertex)))
    (stroke-and-fill stroke-color fill-color thickness)))


(defun draw-polyline (points color &key (thickness 1.0))
  (path
    (move-to (car points))
    (loop for point in (rest points)
       do (%nvg:line-to *canvas* (x point) (y point)))
    (stroke-color color)
    (stroke-width thickness)
    (stroke-path)))
