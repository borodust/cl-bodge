(cl:in-package :cl-bodge.resources)

;;;
;;;
;;;
(defenum pixel-format
  :grey :rgb :rgba)

;;;
;;;
;;;
(defgeneric image-pixel-format (image))
(defgeneric image-width (image))
(defgeneric image-height (image))
(defgeneric image->foreign-array (image))


(defclass image ()
  ((width :initarg :width :reader image-width)
   (height :initarg :height :reader image-height)
   (format :initarg :pixel-format :reader image-pixel-format)
   (data :initarg :data :reader image->foreign-array)))


(defun pixel-format->channels (pixel-format)
  (ecase pixel-format
    (:grey 1)
    (:rgb 3)
    (:rgba 4)))


(defun prepare-png-data (width height pixel-format data)
  (loop with channels = (pixel-format->channels pixel-format)
        with result = (make-foreign-array (* width height channels) :element-type '(unsigned-byte 8))
        with array = (simple-array-of result)
        for i from 0 below height
        do (loop for j from 0 below width
                 do (if (= channels 1)
                        (setf (aref array (+ j (* i width)))
                              (aref data i j))
                        (loop for k from 0 below channels
                              do (setf (aref array (+ k (* j channels) (* (- height i 1) width channels)))
                                       (aref data i j k)))))
        finally (return result)))


(defun unwind-png-data (image data)
  (let ((width (image-width image))
        (height (image-height image))
        (array (simple-array-of (image->foreign-array image))))
    (loop with channels = (pixel-format->channels (image-pixel-format image))
          for i from 0 below height
          do (loop for j from 0 below width
                   do (if (= channels 1)
                          (setf (aref data i j)
                                (aref array (+ j (* i width))))
                          (loop for k from 0 below channels do
                            (setf (aref data i j k)
                                  (aref array (+ k (* j channels) (* (- height i 1) width channels))))))))
    data))




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


(defun write-image-to-stream (stream image type)
  (let* ((writer (ecase type
                   (:png #'opticl:write-png-stream)
                   (:jpeg #'opticl:write-jpeg-stream)
                   (:tiff #'opticl:write-tiff-stream)
                   (:gif #'opticl:write-gif-stream)))
         (image-ctor (ecase (image-pixel-format image)
                       (:grey #'opticl:make-8-bit-gray-image)
                       (:rgb #'opticl:make-8-bit-rgb-image)
                       (:rgba #'opticl:make-8-bit-rgba-image)))
         (opticl-data (funcall image-ctor
                               (image-height image)
                               (image-width image))))
    (unwind-png-data image opticl-data)
    (funcall writer stream opticl-data)))


(defun convert-to-rgba (image &optional (alpha 1f0))
  (let ((alpha (the (unsigned-byte 8)
                    (round (alexandria:clamp (* alpha 255) 0 255)))))
    (labels ((%expand-grey (src dst src-idx dst-idx)
               (let ((color (aref src src-idx)))
                 (setf (aref dst (+ dst-idx 0)) color
                       (aref dst (+ dst-idx 1)) color
                       (aref dst (+ dst-idx 2)) color
                       (aref dst (+ dst-idx 3)) alpha)))
             (%expand-rgb (src dst src-idx dst-idx)
               (setf (aref dst (+ dst-idx 0)) (aref src (+ src-idx 0))
                     (aref dst (+ dst-idx 1)) (aref src (+ src-idx 1))
                     (aref dst (+ dst-idx 2)) (aref src (+ src-idx 2))
                     (aref dst (+ dst-idx 3)) alpha))
             (%convert (image)
               (let* ((source (simple-array-of (image->foreign-array image)))
                      (width (image-width image))
                      (height (image-height image))
                      (pixel-format (image-pixel-format image))
                      (channels (pixel-format->channels pixel-format))
                      (expander (ecase (image-pixel-format image)
                                  (:grey #'%expand-grey)
                                  (:rgb #'%expand-rgb)))
                      (target (make-foreign-array (* width height 4))))
                 (loop with destination = (simple-array-of target)
                       for i from 0 below (* width height)
                       do (funcall expander
                                   source destination
                                   (* i channels) (* i 4))
                       finally (return target)))))
      (if (eq (image-pixel-format image) :rgba)
          image
          (make-instance 'image
                         :data (%convert image)
                         :width (image-width image)
                         :height (image-height image)
                         :pixel-format :rgba)))))


(defun load-png-image (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (read-image-from-stream stream :png)))


(defun load-jpeg-image (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (read-image-from-stream stream :jpeg)))


;;;
;;; Image resource
;;;
(defclass image-resource-handler ()
  ((type :initarg :type :initform (error ":type missing"))))


(defmethod handler-resource-type ((this image-resource-handler))
  (with-slots (type) this
    (ecase type
      (:png :png-image)
      (:jpeg :jpeg-image))))


(defmethod decode-resource ((this image-resource-handler) stream)
  (with-slots (type) this
    (read-image-from-stream stream type)))


(defmethod encode-resource ((this image-resource-handler) resource stream)
  (with-slots (type) this
    (write-image-to-stream stream resource type)))


(defun make-image-resource-handler (image-type)
  (make-resource-handler :image :type image-type))


(defmethod make-resource-handler ((type (eql :image)) &key ((:type image-type)
                                                            (error ":type missing")))
  (make-instance 'image-resource-handler :type image-type))


(defmethod make-resource-handler ((type (eql :png-image)) &key)
  (make-resource-handler :image :type :png))


(defmethod make-resource-handler ((type (eql :jpeg-image)) &key)
  (make-resource-handler :image :type :jpeg))
