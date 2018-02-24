(cl:in-package :cl-bodge.canvas)


(defclass nvg-image (disposable)
  ((context :initform nil)
   (data :initform nil)
   (id :initform nil :reader id-of)))


(define-destructor nvg-image (id context data)
  (dispose data)
  (nvg:destroy-image context id))


(defmethod initialize-instance :after ((this nvg-image) &key image canvas
                                                          flip-vertically
                                                          use-nearest-interpolation)
  (assert (eq :rgba (pixel-format-of image)) (image)
          "Only :rgba images supported")
  (with-slots (context id data) this
    (let ((ctx (handle-value-of canvas))
          (array (simple-array-of (foreign-array-of image)))
          (opts (nconc (list :generate-mipmaps :repeatx :repeaty)
                       (when flip-vertically
                         (list :flipy))
                       (when use-nearest-interpolation
                         (list :nearest)))))
      (setf data (make-foreign-array (length array) :initial-contents array))
      (setf context ctx
            id (apply #'nvg:make-rgba-image ctx (width-of image) (height-of image)
                      (foreign-pointer-of data)
                      opts)))))


(defun image->nvg (canvas image &key flip-vertically use-nearest-interpolation)
  (make-instance 'nvg-image
                 :image image
                 :canvas canvas
                 :use-nearest-interpolation use-nearest-interpolation
                 :flip-vertically flip-vertically))
