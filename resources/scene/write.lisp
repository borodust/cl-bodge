(cl:in-package :cl-bodge.resources)


(defun write-sequence-bytes (stream source element-size)
  (let ((byte-length (* (reduce #'* (array-dimensions source)) element-size)))
    (with-simple-array-pointer (source-ptr source)
      (with-static-vectors ((byte-vec byte-length :element-type '(unsigned-byte 8)))
        (claw:memcpy (static-vector-pointer byte-vec) source-ptr
                     :n byte-length :type :unsigned-char)
        (write-sequence byte-vec stream)))))


(defun write-descriptor (stream type &rest params &key &allow-other-keys)
  (with-character-stream (stream)
    (prin1 (nconc (list type) params) stream)))


(defun write-struct (stream type &rest params &key &allow-other-keys)
  (with-character-stream (stream)
    (prin1 (nconc (list type) params) stream)))


(defun array-type-info (source)
  (eswitch ((array-element-type source) :test #'subtypep)
    ('single-float (values :float (claw:sizeof :float)))
    ('(unsigned-byte 32) (values :unsigned-int (claw:sizeof :int)))))


(defun write-array (out type array &rest params &key &allow-other-keys)
  (let ((length (reduce #'* (array-dimensions array))))
    (when (> length 0)
      (multiple-value-bind (element-type element-size) (array-type-info array)
        (apply #'write-descriptor out type :length length :type element-type params)
        (write-sequence-bytes out array element-size)))))

;;;
;;; MESHES
;;;
(defun write-mesh-attributes (out mesh)
  (write-array out :position-array (mesh-resource-position-array mesh))
  (write-array out :index-array (mesh-resource-index-array mesh))
  (write-array out :normal-array (mesh-resource-normal-array mesh))
  (write-array out :tangent-array (mesh-resource-tangent-array mesh))
  (do-mesh-resource-color-arrays (array channel-id mesh)
    (write-array out :color-array array :channel-id channel-id))
  (do-mesh-resource-tex-coord-arrays (array channel-id mesh)
    (write-array out :tex-coord-array array :channel-id channel-id)))


(defun write-mesh (out mesh mesh-idx)
  (let ((data (flex:with-output-to-sequence (stream)
                (write-mesh-attributes stream mesh))))
    (write-descriptor out :mesh :index mesh-idx
                                :primitive (mesh-resource-primitive mesh)
                                :size (length data))
    (write-sequence data out)))


(defun write-meshes (out scene)
  (do-scene-resource-meshes (mesh id scene)
    (write-mesh out mesh id)))


(defun write-materials (stream scene)
  (do-scene-resource-materials (material id scene)
    (let (tex-list)
      (do-material-resource-textures (tex type id material)
        (push (list :type type
                    :id id
                    :name (namestring (texture-resource-name tex))
                    :channel (texture-resource-channel tex)
                    :coord-id (texture-resource-coord-id tex)
                    :mapping-id (texture-resource-mapping-id tex)
                    :mapping-name (texture-resource-mapping-name tex)
                    :mapping-mode-u (texture-resource-mapping-mode-u tex)
                    :mapping-mode-v (texture-resource-mapping-mode-v tex)
                    :mapping-filter-mag (texture-resource-mapping-filter-mag tex)
                    :mapping-filter-min (texture-resource-mapping-filter-min tex)
                    :scale (texture-resource-scale tex)
                    :strength (texture-resource-strength tex))
              tex-list))
      (write-descriptor stream :material
                        :index id
                        :name (namestring (material-resource-name material))
                        :shininess (material-resource-shininess material)
                        :diffuse-color (vec->array (material-resource-diffuse-color material))
                        :emissive-color (vec->array (material-resource-emissive-color material))
                        :base-color-factor (vec->array (material-resource-base-color-factor material))
                        :metallic-factor (material-resource-metallic-factor material)
                        :roughness-factor (material-resource-roughness-factor material)
                        :glossiness-factor (material-resource-glossiness-factor material)
                        :alpha-mode (material-resource-alpha-mode material)
                        :alpha-cutoff (material-resource-alpha-cutoff material)
                        :textures tex-list))))


(defun write-scene (stream scene)
  (write-meshes stream scene)
  (write-materials stream scene))
