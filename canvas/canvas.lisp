(cl:in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(defvar *black* (vec4 0 0 0 1))


(defgeneric %handle-of (object))


(defclass canvas (disposable)
  ((handle :initarg :handle :reader %handle-of)
   (gx-ctx :initarg :context)))


(define-destructor canvas (handle gx-ctx)
  (run (for-graphics :disposing t :context gx-ctx()
         (bodge-canvas:destroy-canvas handle))))


(define-system-function make-canvas graphics-system
    (canvas-class width height &key (pixel-ratio 1.0) (antialiased t))
  (make-instance canvas-class
                 :handle (bodge-canvas:make-canvas width height :pixel-ratio pixel-ratio
                                                                :antialiased antialiased)
                 :context (graphics-context)))

(defun render-canvas (canvas renderer)
  (let ((*canvas* canvas))
    (bodge-canvas:with-preserved-state (:blend-enabled t
                                        :cull-face-enabled t
                                        :depth-test-enabled t
                                        :cull-face :back
                                        :blend-func '(:src-alpha :one-minus-src-alpha))
      (bodge-canvas:with-canvas ((%handle-of canvas))
        (funcall renderer)))))


(defmacro defcanvas (name-and-opts (&rest parameters) &body body)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    (with-gensyms (this)
      `(progn
         (defclass ,name (canvas) ())
         (defmethod ge.gx::render-pipeline ((,this ,name) &key ,@parameters)
           (flet ((%render ()
                    ,@body))
             (render-canvas ,this #'%render)))))))


(defun canvas-width (&optional (canvas *canvas*))
  (bodge-canvas:canvas-width (%handle-of canvas)))


(defun canvas-height (&optional (canvas *canvas*))
  (bodge-canvas:canvas-height (%handle-of canvas)))


(defun canvas-font-metrics (&optional (canvas *canvas*))
  (bodge-canvas:canvas-font-metrics (%handle-of canvas)))


(defun canvas-text-bounds (string &optional (canvas *canvas*))
  (bodge-canvas:canvas-text-bounds string (%handle-of canvas)))


(defun canvas-text-advance (string &optional (canvas *canvas*))
  (bodge-canvas:canvas-text-advance string (%handle-of canvas)))


(defun register-font-face (canvas name font-container)
  (bodge-canvas:register-font-face (%handle-of canvas)
                                   name (ge.rsc:font-container-data font-container)))


(defun update-canvas-size (canvas width height)
  (bodge-canvas:update-canvas-size (%handle-of canvas) width height))


(defun update-canvas-pixel-ratio (canvas pixel-ratio)
  (bodge-canvas:update-canvas-pixel-ratio (%handle-of canvas) pixel-ratio))


(defmacro with-blend-factors ((source-factor destination-factor
                               &optional source-alpha-factor destination-alpha-factor)
                              &body body)
  `(bodge-canvas::with-blend-factors (,source-factor ,destination-factor
                                                       :source-alpha-factor
                                                       ,source-alpha-factor
                                                       :destination-alpha-factor
                                                       ,destination-alpha-factor
                                                       :canvas (%handle-of *canvas*))
     ,@body))


(defmacro with-composite-operation ((operation) &body body)
  `(bodge-canvas:with-composite-operation (,operation (%handle-of *canvas*))
     ,@body))


(defmacro with-alpha ((value &key override) &body body)
  `(bodge-canvas:with-alpha (,value :override ,override :canvas (%handle-of *canvas*))
     ,@body))
