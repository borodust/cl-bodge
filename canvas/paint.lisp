(in-package :cl-bodge.canvas)


(defgeneric (setf fill-paint) (value canvas))
(defgeneric (setf stroke-paint) (value canvas))


(defmethod (setf fill-paint) ((color vec4) (canvas canvas))
  (fill-color color canvas))


(defmethod (setf stroke-paint) ((color vec4) (canvas canvas))
  (stroke-color color canvas))


(defhandle paint-handle
    :initform  (alloc '(:struct (%nvg:paint)))
    :closeform (free *handle-value*))


(defclass image-paint (foreign-object)
  ((image :initform nil)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of))
  (:default-initargs :handle (make-paint-handle)))


(define-destructor image-paint (image)
  (dispose image))


(defmethod initialize-instance :after ((this image-paint) &key image (canvas *canvas*)
                                                            origin flip-vertically
                                                            use-nearest-interpolation)
  (with-slots ((img image) width height) this
    (setf img (image->nvg canvas image
                          :flip-vertically flip-vertically
                          :use-nearest-interpolation use-nearest-interpolation)
          width (width-of image)
          height (height-of image))
    (%nvg:bge-init-image-pattern (handle-value-of this)
                                 (handle-value-of canvas)
                                 (if origin (x origin) 0.0)
                                 (if origin (y origin) 0.0)
                                 (f (width-of image)) (f (height-of image))
                                 0.0 (id-of img) 1.0)))


(defun make-image-paint (image &key origin (canvas *canvas*) flip-vertically
                                 use-linear-interpolation)
  (make-instance 'image-paint
                 :image image
                 :canvas canvas
                 :origin origin
                 :use-nearest-interpolation (not use-linear-interpolation)
                 :flip-vertically flip-vertically))


(defmethod (setf fill-paint) ((value image-paint) (canvas canvas))
  (%nvg:bge-fill-paint (handle-value-of canvas) (handle-value-of value)))


(defmethod (setf stroke-paint) ((value image-paint) (canvas canvas))
  (%nvg:bge-fill-paint (handle-value-of canvas) (handle-value-of value)))
