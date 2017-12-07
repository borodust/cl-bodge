(in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(defhandle canvas-handle
    :closeform (bodge-nanovg:destroy-context *handle-value*))


(defclass canvas (foreign-object)
  ((width :initarg :width :initform (error ":width missing") :reader width-of)
   (height :initarg :height :initform (error ":height missing") :reader height-of)
   (pixel-ratio :initarg :pixel-ratio :initform 1.0 :reader pixel-ratio-of)
   (font-map :initform (make-hash-table :test #'equal))))


(defun %register-font (canvas name font-data)
  (with-slots (font-map) canvas
    (setf (gethash (namestring name) font-map) font-data)))


(defun update-canvas-size (canvas width height)
  (with-slots ((w width) (h height)) canvas
    (setf w width
          h height)))


(define-system-function make-canvas graphics-system (width height &key (pixel-ratio 1.0) antialiased)
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased (list :antialias))
                      (in-development-mode (list :debug)))))
    (make-instance 'canvas
                   :handle (make-canvas-handle (apply #'bodge-nanovg:make-context opts))
                   :pixel-ratio pixel-ratio
                   :width width
                   :height height)))


(defun %invert-coordinate-system (canvas)
  (%nvg:translate (handle-value-of canvas) 0f0 (f (height-of canvas)))
  (%nvg:scale (handle-value-of canvas) 1f0 -1f0))


(defun %restore-coordinate-system (canvas)
  (%nvg:scale (handle-value-of canvas) 1f0 -1f0)
  (%nvg:translate (handle-value-of canvas) 0f0 (f (- (height-of canvas)))))


(defun begin-canvas (canvas)
  (%nvg:begin-frame (handle-value-of canvas) (width-of canvas) (height-of canvas)
                    (pixel-ratio-of canvas))
  (%invert-coordinate-system canvas))


(defun end-canvas (canvas)
  (%nvg:end-frame (handle-value-of canvas))
  (reset-state))


(defun flush-canvas (&optional (canvas *canvas*))
  (end-canvas canvas)
  (begin-canvas canvas))


(defmacro with-canvas ((canvas) &body body)
  (once-only (canvas)
    `(let ((*canvas* ,canvas))
       (unwind-protect
            (progn
              (begin-canvas *canvas*)
              ,@body)
         (end-canvas *canvas*)))))


(definline fill-path (&optional (canvas *canvas*))
  (%nvg:fill (handle-value-of canvas)))


(definline stroke-path (&optional (canvas *canvas*))
  (%nvg:stroke (handle-value-of canvas)))


(definline move-to (coords &optional (canvas *canvas*))
  (%nvg:move-to (handle-value-of canvas) (x coords) (y coords)))


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


(defun push-canvas (&optional (canvas *canvas*))
  (%nvg:save (handle-value-of canvas)))


(defun pop-canvas (&optional (canvas *canvas*))
  (%nvg:restore (handle-value-of canvas)))


(defun reset-canvas (&optional (canvas *canvas*))
  (%nvg:reset (handle-value-of canvas)))


(defmacro with-pushed-canvas ((&optional canvas) &body body)
  (with-gensyms (cvs)
    `(let ((,cvs ,(or canvas '*canvas*)))
       (unwind-protect
            (progn
              (push-canvas ,cvs)
              ,@body)
         (pop-canvas ,cvs)))))
