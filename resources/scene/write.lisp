(cl:in-package :cl-bodge.resources)


(defun write-sequence-bytes (stream source element-size)
  (let ((byte-length (* (reduce #'* (array-dimensions source)) element-size)))
    (with-simple-array-pointer (source-ptr source)
      (with-static-vectors ((byte-vec byte-length :element-type '(unsigned-byte 8)))
        (claw:memcpy (static-vector-pointer byte-vec) source-ptr byte-length)
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
    ('(unsigned-byte 32) (values :unsigned-int (claw:sizeof :unsigned-int)))
    ('(signed-byte 32) (values :int (claw:sizeof :int)))))


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
  (write-array out :bone-id-array (mesh-resource-bone-id-array mesh))
  (write-array out :bone-weight-array (mesh-resource-bone-weight-array mesh))
  (do-mesh-resource-color-arrays (array channel-id mesh)
    (write-array out :color-array array :channel-id channel-id))
  (do-mesh-resource-tex-coord-arrays (array channel-id mesh)
    (write-array out :tex-coord-array array :channel-id channel-id))
  (loop for bone across (mesh-resource-bone-array mesh)
        for bone-index from 0
        do (write-descriptor out :bone :index bone-index
                                       :node-id (mesh-resource-bone-node-id bone)
                                       :offset (mat->array (mesh-resource-bone-offset bone)))))


(defun write-mesh (out mesh mesh-idx)
  (write-descriptor out :mesh :index mesh-idx
                              :primitive (mesh-resource-primitive mesh))
  (write-mesh-attributes out mesh))


(defun write-meshes (out scene)
  (do-scene-resource-meshes (mesh id scene)
    (write-mesh out mesh id)))


(defun vec-to-array (vec)
  (when vec
    (vec->array vec)))


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
                        :name (namestring (or (material-resource-name material) ""))
                        :shininess (material-resource-shininess material)
                        :diffuse-color (vec-to-array (material-resource-diffuse-color material))
                        :emissive-color (vec-to-array (material-resource-emissive-color material))
                        :base-color-factor (vec-to-array (material-resource-base-color-factor material))
                        :metallic-factor (material-resource-metallic-factor material)
                        :roughness-factor (material-resource-roughness-factor material)
                        :glossiness-factor (material-resource-glossiness-factor material)
                        :alpha-mode (material-resource-alpha-mode material)
                        :alpha-cutoff (material-resource-alpha-cutoff material)
                        :textures tex-list))))


(defun prepare-metadata (alist)
  (flet ((convert-metadata (cons)
           (destructuring-bind (key . value) cons
             (cons key (typecase value
                         (vec (list :vec3 (vec->array value)))
                         (t (list :t value)))))))
    (mapcar #'convert-metadata alist)))


(defun write-node (out node)
  (let ((node-id (scene-resource-node-id node)))
    (write-descriptor out :node :id (scene-resource-node-id node)
                                :transform (mat->array (scene-resource-node-transform node))
                                :mesh-id-list (scene-resource-node-mesh-id-list node)
                                :children (loop for child in (scene-resource-node-children node)
                                                collect (write-node out child))
                                :metadata (prepare-metadata
                                           (alexandria:hash-table-alist
                                            (scene-resource-node-metadata-table node))))
    node-id))


(defun write-node-graph (out root-node)
  (write-descriptor out :node-graph :root-node-id (scene-resource-node-id root-node))
  (write-node out root-node))


(defun write-animation-channel-sequence (out type key-sequence)
  (write-array out :keyframe-timing-array (animation-resource-key-sequence-timing-array key-sequence)
                   :transform-type type)
  (write-array out :keyframe-value-array (animation-resource-key-sequence-value-array key-sequence)
                   :transform-type type))


(defun write-animation-channel (out channel)
  (write-descriptor out :animation-channel :node-id (animation-resource-channel-node-id channel)
                                           :pre-state (animation-resource-channel-pre-state channel)
                                           :post-state (animation-resource-channel-post-state channel))
  (let ((rotation (animation-resource-channel-rotation-sequence channel))
        (translation (animation-resource-channel-translation-sequence channel))
        (scale (animation-resource-channel-scale-sequence channel)))
    (write-animation-channel-sequence out :rotation rotation)
    (write-animation-channel-sequence out :translation translation)
    (write-animation-channel-sequence out :scale scale)))


(defun write-animations (out scene)
  (do-scene-resource-animations (animation track-id scene)
    (write-descriptor out :animation :id track-id
                                     :duration (%animation-resource-duration animation))
    (loop for channel in (animation-resource-channels animation)
          do (write-animation-channel out channel))))


(defun write-scene (stream scene)
  (write-meshes stream scene)
  (write-materials stream scene)
  (write-node-graph stream (scene-resource-root-node scene))
  (write-animations stream scene))
