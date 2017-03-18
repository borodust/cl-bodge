(in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(definline %invert (y canvas &optional (h 0.0))
  (- (height-of canvas) y h))


(defhandle canvas-handle
    :closeform (bodge-nanovg:destroy-context *handle-value*))


(defclass canvas (foreign-object)
  ((width :initarg :width :initform (error ":width missing") :reader width-of)
   (height :initarg :height :initform (error ":height missing") :reader height-of)))


(defun update-canvas-size (canvas width height)
  (with-slots ((w width) (h height)) canvas
    (setf w width
          h height)))


(define-system-function make-canvas graphics-system (width height &key antialiased-p)
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased-p (list :antialias))
                      (when-debugging (list :debug)))))
    (make-instance 'canvas
                   :handle (make-canvas-handle (apply #'bodge-nanovg:make-context opts))
                   :width width
                   :height height)))


(definline begin-canvas (canvas &optional (pixel-ratio 1.0))
  (%nvg:begin-frame (handle-value-of canvas) (width-of canvas) (height-of canvas) pixel-ratio))


(definline end-canvas (canvas)
  (%nvg:end-frame (handle-value-of canvas)))


(defmacro with-canvas ((canvas &optional (pixel-ratio 1.0)) &body body)
  (once-only (canvas)
    `(preserving-state
       (let ((*canvas* ,canvas))
         (unwind-protect
              (progn
                (begin-canvas ,canvas ,pixel-ratio)
                ,@body)
           (end-canvas ,canvas))))))


(definline fill-path (&optional (canvas *canvas*))
  (%nvg:fill (handle-value-of canvas)))


(definline stroke-path (&optional (canvas *canvas*))
  (%nvg:stroke (handle-value-of canvas)))


(definline move-to (coords &optional (canvas *canvas*))
  (%nvg:move-to (handle-value-of canvas) (x coords) (%invert (y coords) canvas)))


(defun stroke-color (color &optional (canvas *canvas*))
  (c-with ((nk-color (:struct (%nk:color))))
    (%nvg:rgba-f nk-color (x color) (y color) (z color) (w color))
    (%nvg:stroke-color (handle-value-of canvas) nk-color)))


(definline stroke-width (width &optional (canvas *canvas*))
  (%nvg:stroke-width (handle-value-of canvas) (f width)))


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
    (%nvg:scissor (handle-value-of canvas) (x origin) (%invert (y origin) canvas h)
                  (f w) (f h))))


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
    (%nvg:line-to (handle-value-of canvas) (x end) (%invert (y end) canvas))
    (stroke-width thickness canvas)
    (stroke-color color canvas)
    (stroke-path canvas)))


(defun draw-curve (origin end ctrl0 ctrl1 color &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to origin canvas)
    (%nvg:bezier-to (handle-value-of canvas)
                    (x ctrl0) (%invert (y ctrl0) canvas)
                    (x ctrl1) (%invert (y ctrl1) canvas)
                    (x end) (%invert (y end) canvas))
    (stroke-color color canvas)
    (stroke-width thickness canvas)
    (stroke-path canvas)))


(defun draw-rect (origin w h &key (fill-color nil) (stroke-color nil)
                               (thickness 1.0) (rounding 0.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:rounded-rect (handle-value-of canvas)
                       (x origin) (%invert (y origin) canvas h) (f w) (f h) (f rounding))
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-circle (center radius &key (fill-color nil) (stroke-color nil)
                                    (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:circle (handle-value-of canvas) (x center) (%invert (y center) canvas) (f radius))
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-ellipse (center x-radius y-radius &key (fill-color nil) (stroke-color nil)
                                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:ellipse (handle-value-of canvas)
                  (x center) (%invert (y center) canvas) (f x-radius) (f y-radius))
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-arc (center radius a0 a1 &key (fill-color nil) (stroke-color nil)
                                       (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:arc (handle-value-of canvas) (x center) (%invert (y center) canvas) (f radius)
              ;; fixme: invert angles here?
              (f a0) (f a1) %nvg:+ccw+)
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-polygon (vertices &key (fill-color nil) (stroke-color nil)
                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (let ((first-vertex (car vertices)))
      (move-to first-vertex canvas)
      (loop for vertex in (rest vertices)
         do (%nvg:line-to (handle-value-of canvas)
                          (x vertex) (%invert (y vertex) canvas)))
      (%nvg:line-to (handle-value-of canvas)
                    (x first-vertex) (%invert (y first-vertex) canvas)))
    (stroke-and-fill stroke-color fill-color thickness canvas)))


(defun draw-polyline (points color &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to (car points) canvas)
    (loop for point in (rest points)
       do (%nvg:line-to (handle-value-of canvas) (x point) (%invert (y point) canvas)))
    (stroke-color color)
    (stroke-width thickness)
    (stroke-path)))
