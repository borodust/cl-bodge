(cl:in-package :cl-bodge.resources)

;;;
;;; Scene resource
;;;
(defclass scene-resource-handler () ())


(defmethod decode-resource ((this scene-resource-handler) stream)
  (read-scene stream))


(defmethod encode-resource ((this scene-resource-handler) (resource scene-resource) stream)
  (write-scene stream resource))


(defmethod make-resource-handler ((type (eql :scene)) &key)
  (make-instance 'scene-resource-handler))
