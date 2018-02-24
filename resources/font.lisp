(cl:in-package :cl-bodge.resources)

;;;
;;; SDF font resource
;;;

(definline sdf-font-resource-name (name item)
  (fad:merge-pathnames-as-file (fad:pathname-as-directory name) item))


(definline sdf-font-atlas-resource-name (name)
  (sdf-font-resource-name name "image"))

(definline sdf-font-metrics-resource-name (name)
  (sdf-font-resource-name name "font"))

;;;
;;; SDF font resource handler
;;;
(defgeneric font-container-data (container))


(defclass sdf-font-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :font-atlas))


(defmacro define-sdf-font (name)
  `(progn
     (defresource :image (sdf-font-atlas-resource-name ,name)
       :type :png)
     (defresource :font (sdf-font-metrics-resource-name ,name)
       :type :sdf)))


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
