(in-package :cl-bodge.canvas)


(defun translate-canvas (x y &key (canvas *canvas*))
  (%nvg:translate (handle-value-of canvas) (f x) (f y)))


(defun rotate-canvas (angle &key (canvas *canvas*))
  (%nvg:rotate (handle-value-of canvas) (f angle)))


(defun skew-canvas (x y &key (canvas *canvas*))
  (%nvg:skew-x (handle-value-of canvas) (f x))
  (%nvg:skew-y (handle-value-of canvas) (f y)))


(defun scale-canvas (x y &key (canvas *canvas*))
  (%nvg:scale (handle-value-of canvas) (f x) (f y)))


(defun reset-canvas-transform (&key (canvas *canvas*))
  (%nvg:reset-transform (handle-value-of canvas)))
