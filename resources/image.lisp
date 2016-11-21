(in-package :cl-bodge.resources)


(defclass image ()
  ((width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (format :initarg :pixel-format :reader ge.gx.rsc:pixel-format-of)))


(defclass png-image (image)
  ((data :initarg :data :reader pixels-of)))


(defun load-png-image (path)
  (let* ((data (opticl:read-png-file path))
         (format (etypecase data
                   (opticl:8-bit-gray-image :grey)
                   (opticl:8-bit-rgb-image :rgb)
                   (opticl:8-bit-rgba-image :rgba))))
    (opticl:with-image-bounds (h w) data
      (make-instance 'png-image
                     :data data
                     :width w
                     :height h
                     :pixel-format format))))


(defmethod ge.gx.rsc:size-of ((this png-image))
  (values (width-of this) (height-of this)))


(defmethod ge.gx.rsc:image->array ((this png-image))
  (loop with width = (width-of this)
     and height = (height-of this)
     and channels = (ecase (ge.gx.rsc:pixel-format-of this)
                      (:grey 1)
                      (:rgb 3)
                      (:rgba 4))
     and data = (pixels-of this)
     with result = (make-array (* width height channels) :element-type '(unsigned-byte 8))
     for i from 0 below height do
       (loop for j from 0 below width do
            (if (= channels 1)
                (setf (aref result (+ j (* i width)))
                       (aref data i j))
                (loop for k from 0 below channels do
                     (setf (aref result (+ k (* j channels) (* (- height i 1) width channels)))
                           (aref data i j k)))))
     finally (return result)))
