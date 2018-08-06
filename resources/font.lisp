(cl:in-package :cl-bodge.resources)

;;;
;;; Conventional fonts
;;;
(defclass font-container ()
  ((data :initarg :data :reader font-container-data)))


(defclass truetype-font-resouce-handler () ())


(defmethod decode-resource ((this truetype-font-resouce-handler) stream)
  (make-instance 'font-container
                 :data (alexandria:read-stream-content-into-byte-vector stream)))


(defmethod encode-resource ((this truetype-font-resouce-handler) (value font-container) stream)
  (write-sequence (font-container-data value) stream))

;;;
;;; Font contstructor
;;;
(defmethod make-resource-handler ((type (eql :font)) &key ((:type font-type)
                                                           (error ":type missing")))
  (eswitch (font-type)
    (:sdf (make-instance 'sdf-font-resource-handler))
    (:ttf (make-instance 'truetype-font-resouce-handler))))


(defmethod make-resource-handler ((type (eql :ttf-font)) &key)
  (make-resource-handler :font :type :ttf))


(defmethod make-resource-handler ((type (eql :font-atlas)) &key)
  (make-resource-handler :font :type :sdf))
