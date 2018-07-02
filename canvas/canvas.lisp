(cl:in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(defhandle canvas-handle
    :closeform (nanovg:destroy-context *handle-value*))

(defvar *default-font-name* "NotoMono-Regular")


(let* ((default-font-data
         (read-file-into-byte-vector
          (system-relative-pathname :cl-bodge/canvas
                                    (format nil "font/~A.ttf" *default-font-name*))))
       (default-font-data-static (make-guarded-reference nil)))
  (defun default-font-static-data ()
    ;; trick to load static-vectorized data upon first usage
    ;; but dump default-font-data as a plain array into an image
    (with-guarded-reference (font-data default-font-data-static)
      (unless font-data
        (setf font-data (static-vectors:make-static-vector (length default-font-data)
                                                           :initial-contents default-font-data)
              default-font-data nil))
      font-data)))


(defclass canvas (foreign-object)
  ((width :initarg :width :initform (error ":width missing") :reader width-of)
   (height :initarg :height :initform (error ":height missing") :reader height-of)
   (pixel-ratio :initarg :pixel-ratio :initform 1.0 :reader pixel-ratio-of)
   (default-font-id :reader %default-font-of)
   (font-map :initform (make-hash-table :test #'equal))))


(defun %register-font (canvas name font-data)
  (with-slots (font-map) canvas
    (setf (gethash (namestring name) font-map) font-data)))


(defun %ensure-font-face (canvas name foreign-data-ptr data-size)
  (let ((font-face-id (%nvg:find-font (handle-value-of canvas) (namestring name))))
    (if (< font-face-id 0)
        (%nvg:create-font-mem (handle-value-of canvas) (namestring name)
                              foreign-data-ptr data-size 0)
        font-face-id)))


(defun %register-font-face (canvas name data)
  (let* ((f-data (static-vectors:make-static-vector (length data) :initial-contents data))
         (id (%ensure-font-face canvas name
                                (static-vectors:static-vector-pointer f-data) (length f-data))))
    (when (< id 0)
      (static-vectors:free-static-vector f-data)
      (error "Failed to register face with name '~A'" name))
    (%register-font canvas name f-data)
    id))


(defun update-canvas-size (canvas width height)
  (with-slots ((w width) (h height)) canvas
    (setf w (floor width)
          h (floor height))))


(defmethod initialize-instance :after ((this canvas) &key)
  (with-slots (default-font-id) this
    (let* ((default-font-data (default-font-static-data))
           (font-id (%ensure-font-face this *default-font-name*
                                      (static-vectors:static-vector-pointer default-font-data)
                                      (length default-font-data))))
      (%nvg:add-fallback-font-id (handle-value-of this) font-id font-id)
      (setf default-font-id font-id))))


(defmacro defcanvas (name-and-opts (&rest key-parameters) &body body)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    (with-gensyms (this)
      `(progn
         (defclass ,name (canvas) ())
         (defmethod ge.gx::render-pipeline ((,this ,name) &key ,@key-parameters)
           (with-canvas (,this)
             ,@body))))))


(define-system-function make-canvas graphics-system (canvas-class width height &key
                                                                  (pixel-ratio 1.0)
                                                                  (antialiased t))
  (let ((opts (append (list :stencil-strokes)
                      (when antialiased (list :antialias))
                      (in-development-mode (list :debug)))))
    (make-instance canvas-class
                   :handle (make-canvas-handle (apply #'nanovg:make-context opts))
                   :pixel-ratio pixel-ratio
                   :width (floor width)
                   :height (floor height))))


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
  (c-with ((color-v %nvg:color))
    (setf (color-v :r) (x color)
          (color-v :g) (y color)
          (color-v :b) (z color)
          (color-v :a) (w color))
    (%nvg:stroke-color (handle-value-of canvas) color-v)))


(defun fill-color (color &optional (canvas *canvas*))
  (c-with ((color-v %nvg:color))
    (setf (color-v :r) (x color)
          (color-v :g) (y color)
          (color-v :b) (z color)
          (color-v :a) (w color))
    (%nvg:fill-color (handle-value-of canvas) color-v)))


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
