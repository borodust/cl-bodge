(in-package :cl-bodge.canvas)


(defun scissors (origin w h &optional (canvas *canvas*))
  (path (canvas)
    (%nvg:scissor (handle-value-of canvas) (x origin) (%invert (y origin) canvas h)
                  (f w) (f h))))


(definline stroke-and-fill (stroke-paint fill-paint thickness canvas)
  (when fill-paint
    (setf (fill-paint canvas) fill-paint)
    (fill-path canvas))
  (when stroke-paint
    (setf (stroke-width canvas) thickness
          (stroke-paint canvas) stroke-paint)
    (stroke-path canvas)))


(defun draw-line (origin end paint &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to origin canvas)
    (%nvg:line-to (handle-value-of canvas) (x end) (%invert (y end) canvas))
    (setf (stroke-width canvas) thickness
          (stroke-paint canvas) paint)
    (stroke-path canvas)))


(defun draw-curve (origin end ctrl0 ctrl1 paint &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to origin canvas)
    (%nvg:bezier-to (handle-value-of canvas)
                    (x ctrl0) (%invert (y ctrl0) canvas)
                    (x ctrl1) (%invert (y ctrl1) canvas)
                    (x end) (%invert (y end) canvas))
    (setf (stroke-paint canvas) paint
          (stroke-width canvas) thickness)
    (stroke-path canvas)))


(defun draw-rect (origin w h &key (fill-paint nil) (stroke-paint nil)
                               (thickness 1.0) (rounding 0.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:rounded-rect (handle-value-of canvas)
                       (x origin) (%invert (y origin) canvas h) (f w) (f h) (f rounding))
    (stroke-and-fill stroke-paint fill-paint thickness canvas)))


(defun draw-circle (center radius &key (fill-paint nil) (stroke-paint nil)
                                    (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:circle (handle-value-of canvas) (x center) (%invert (y center) canvas) (f radius))
    (stroke-and-fill stroke-paint fill-paint thickness canvas)))


(defun draw-ellipse (center x-radius y-radius &key (fill-paint nil) (stroke-paint nil)
                                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:ellipse (handle-value-of canvas)
                  (x center) (%invert (y center) canvas) (f x-radius) (f y-radius))
    (stroke-and-fill stroke-paint fill-paint thickness canvas)))


(defun draw-arc (center radius a0 a1 &key (fill-paint nil) (stroke-paint nil)
                                       (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (%nvg:arc (handle-value-of canvas) (x center) (%invert (y center) canvas) (f radius)
              (f (- a0)) (f (- a1)) %nvg:+ccw+)
    (stroke-and-fill stroke-paint fill-paint thickness canvas)))


(defun draw-polygon (vertices &key (fill-paint nil) (stroke-paint nil)
                                (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (let ((first-vertex (car vertices)))
      (move-to first-vertex canvas)
      (loop for vertex in (rest vertices)
         do (%nvg:line-to (handle-value-of canvas)
                          (x vertex) (%invert (y vertex) canvas)))
      (%nvg:line-to (handle-value-of canvas)
                    (x first-vertex) (%invert (y first-vertex) canvas)))
    (stroke-and-fill stroke-paint fill-paint thickness canvas)))


(defun draw-polyline (points paint &key (thickness 1.0) (canvas *canvas*))
  (path (canvas)
    (move-to (car points) canvas)
    (loop for point in (rest points)
       do (%nvg:line-to (handle-value-of canvas) (x point) (%invert (y point) canvas)))
    (setf (stroke-paint canvas) paint
          (stroke-width canvas) thickness)
    (stroke-path)))
