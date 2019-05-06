(cl:in-package :cl-bodge.canvas)


(defgeneric %paint-handle-of (object))


(defmethod %paint-handle-of ((object vec))
  object)


(defmethod %paint-handle-of ((object null))
  nil)


(defclass image-paint (disposable)
  ((canvas :initarg :canvas)
   (handle :initarg :handle :reader %paint-handle-of)))


(define-destructor image-paint (canvas handle)
  (run (for-graphics :disposing t ()
         (bodge-canvas:destroy-image-paint (%handle-of canvas) handle))))


(define-system-function make-image-paint graphics-system
    (canvas image &key flip-vertically use-nearest-interpolation)
  (unless (eq (ge.rsc:image-pixel-format image) :rgba)
    (error "Only RGBA images supported"))
  (make-instance 'image-paint
                 :canvas canvas
                 :handle (bodge-canvas:make-rgba-image-paint
                          (%handle-of canvas)
                          (simple-array-of
                           (ge.rsc:image->foreign-array image))
                          (ge.rsc:image-width image)
                          (ge.rsc:image-height image)
                          :flip-vertically flip-vertically
                          :use-nearest-interpolation use-nearest-interpolation)))


(defun image-paint-height (paint)
  (bodge-canvas:image-paint-height (%paint-handle-of paint)))


(defun image-paint-width (paint)
  (bodge-canvas:image-paint-width (%paint-handle-of paint)))


(defun (setf fill-paint) (paint)
  (setf (bodge-canvas:fill-paint) (%paint-handle-of paint)))


(defun (setf stroke-paint) (paint)
  (setf (bodge-canvas:stroke-paint) (%paint-handle-of paint)))
