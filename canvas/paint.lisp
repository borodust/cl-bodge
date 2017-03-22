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
  ((image :initform nil))
  (:default-initargs :handle (make-paint-handle)))


(define-destructor image-paint (image)
  (dispose image))


(defmethod initialize-instance :after ((this image-paint) &key image (canvas *canvas*)
                                                            origin flip-vertically)
  (with-slots ((img image)) this
    (setf img (image->nvg canvas image :flip-vertically flip-vertically))
    (%nvg:image-pattern (handle-value-of this)
                        (handle-value-of canvas)
                        (when origin (x origin)) (when origin (%invert (y origin) canvas))
                        (f (width-of image)) (f (height-of image))
                        0.0 (id-of img) 1.0)))


(defun make-image-paint (image &key origin (canvas *canvas*) flip-vertically)
  (make-instance 'image-paint
                 :image image
                 :canvas canvas
                 :origin origin
                 :flip-vertically flip-vertically))


(defmethod (setf fill-paint) ((value image-paint) (canvas canvas))
  (%nvg:fill-paint (handle-value-of canvas) (handle-value-of value)))


(defmethod (setf stroke-paint) ((value image-paint) (canvas canvas))
  (%nvg:fill-paint (handle-value-of canvas) (handle-value-of value)))
