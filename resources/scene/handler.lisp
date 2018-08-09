(cl:in-package :cl-bodge.resources)

;;;
;;; Scene resource
;;;
(defclass scene-resource-handler (resource-handler) ()
  (:default-initargs :resource-type :scene))


(defmethod resource-dependencies ((this scene-resource-handler) scene) ()
  (let (texture-names)
    (do-scene-resource-materials (material id scene)
      (do-material-resource-textures (texture type id material)
        (pushnew (texture-resource-name texture) texture-names :test #'equal)))
    texture-names))


(defmethod decode-resource ((this scene-resource-handler) stream)
  (read-scene stream))


(defmethod encode-resource ((this scene-resource-handler) (resource scene-resource) stream)
  (write-scene stream resource))


(defmethod make-resource-handler ((type (eql :scene)) &key)
  (make-instance 'scene-resource-handler))
