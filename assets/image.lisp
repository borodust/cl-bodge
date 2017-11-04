(in-package :cl-bodge.assets)


(defclass image ()
  ((width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (format :initarg :pixel-format :reader pixel-format-of)
   (data :initarg :data :reader data-of)))


(defun prepare-png-data (width height pixel-format data)
  (loop with channels = (ecase pixel-format
                          (:grey 1)
                          (:rgb 3)
                          (:rgba 4))
        with result = (make-foreign-array (* width height channels)
                                          :element-type '(unsigned-byte 8))
        with array = (simple-array-of result)
     for i from 0 below height do
       (loop for j from 0 below width do
            (if (= channels 1)
                (setf (aref array (+ j (* i width)))
                       (aref data i j))
                (loop for k from 0 below channels do
                     (setf (aref array (+ k (* j channels) (* (- height i 1) width channels)))
                           (aref data i j k)))))
     finally (return result)))


(defun read-image-from-stream (stream type)
  (let* ((data (opticl:read-image-stream stream type))
         (format (etypecase data
                   (opticl:8-bit-gray-image :grey)
                   (opticl:8-bit-rgb-image :rgb)
                   (opticl:8-bit-rgba-image :rgba))))
    (opticl:with-image-bounds (h w) data
      (make-instance 'image
                     :data (prepare-png-data w h format data)
                     :width w
                     :height h
                     :pixel-format format))))


(defun load-png-image (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (read-image-from-stream stream :png)))


(defmethod foreign-array-of ((this image))
  (data-of this))


;;;
;;; Image resource
;;;
(defclass image-resource-handler ()
  ((type :initarg :type :initform (error ":type missing"))))


(defmethod decode-resource ((this image-resource-handler) stream)
  (with-slots (type) this
    (read-image-from-stream stream type)))


(defmethod make-resource-handler ((type (eql :image)) &key ((:type image-type)
                                                            (error ":type missing")))
  (make-instance 'image-resource-handler :type image-type))
