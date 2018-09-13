(cl:in-package :cl-bodge.canvas)


(declaim (special *canvas*))


(defclass canvas (disposable)
  ((handle :initarg :handle :reader %handle-of)
   (image-cache :initform (make-hash-table :test #'eq))))


(define-destructor canvas (handle image-cache)
  (run (for-graphics ()
         (flet ((%destroy-image (image value)
                  (declare (ignore value))
                  (when (remhash image image-cache)
                    (bodge-canvas:destroy-image handle image))))
           (maphash #'%destroy-image image-cache))
         (bodge-canvas:destroy-canvas handle))))


(define-system-function make-canvas graphics-system
    (canvas-class width height &key (pixel-ratio 1.0) (antialiased t))
  (make-instance canvas-class
                 :handle (bodge-canvas:make-canvas width height :pixel-ratio pixel-ratio
                                                                :antialiased antialiased)))

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


(defun make-canvas-image (canvas image)
  (with-slots (image-cache) canvas
    (let ((image (bodge-canvas:make-rgba-image (%handle-of canvas)
                                               (simple-array-of (ge.rsc:image->foreign-array image))
                                               (ge.rsc:image-width image)
                                               (ge.rsc:image-height image))))
      (setf (gethash image image-cache) image))))


(defun destroy-canvas-image (canvas image)
  (with-slots (image-cache) canvas
    (when (remhash image image-cache)
      (run (for-graphics ()
             (bodge-canvas:destroy-image (%handle-of canvas) image))))))


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


(define-system-function make-image-paint graphics-system (canvas image)
  (let* ((image (make-canvas-image canvas image))
         (paint (bodge-canvas:make-image-paint image)))
    (flet ((%destroy-image ()
             (destroy-canvas-image canvas image)))
      (trivial-garbage:finalize paint #'%destroy-image))))
