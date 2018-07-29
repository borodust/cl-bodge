(cl:in-package :cl-bodge.resources)

;;;
;;; SCENE
;;;
(defclass scene-resource ()
  ((mesh-table :initform (make-hash-table) :reader %mesh-table-of)
   (material-table :initform (make-hash-table) :reader %material-table-of)))


(defun make-empty-scene-resource ()
  (make-instance 'scene-resource))


(defun scene-resource-mesh (scene id)
  (with-slots (mesh-table) scene
    (gethash id mesh-table)))


(defun (setf scene-resource-mesh) (mesh scene id)
  (with-slots (mesh-table) scene
    (setf (gethash id mesh-table) mesh)))


(defun scene-resource-material (scene id)
  (with-slots (material-table) scene
    (gethash id material-table)))


(defun (setf scene-resource-material) (material scene id)
  (with-slots (material-table) scene
    (setf (gethash id material-table) material)))


(defmacro do-meshes ((mesh id scene) &body body)
  `(maphash (lambda (,id ,mesh) ,@body) (%mesh-table-of ,scene)))


(defmacro do-materials ((material id scene) &body body)
  `(maphash (lambda (,id ,material) ,@body) (%material-table-of ,scene)))

;;;
;;; MESH
;;;
(defstruct (mesh-resource
            (:constructor %make-mesh-resource))
  primitive
  position-array
  index-array
  normal-array)


(defun make-mesh-resource (primitive)
  (%make-mesh-resource :primitive primitive))


;;;
;;; MATERIAL
;;;
(defstruct texture-resource
  name
  channel
  coord-id
  mapping-id
  mapping-name
  mapping-mode-u
  mapping-mode-v
  mapping-filter-mag
  mapping-filter-min
  scale
  strength)


(defstruct material-resource
  name
  shininess
  diffuse-color
  emissive-color
  ;; metallic-roughness
  base-color-factor
  metallic-factor
  roughness-factor
  glossiness-factor

  alpha-mode
  alpha-cutoff
  (texture-table (make-hash-table :test #'equal)))


(defun material-resource-texture (material type id &optional ensure-exist)
  (let* ((key (cons type id))
         (tex-table (material-resource-texture-table material))
         (texture (gethash key tex-table)))
    (if (and (null texture) ensure-exist)
        (setf (gethash key tex-table) (make-texture-resource))
        texture)))


(defun (setf material-resource-texture) (value material type id)
  (setf (gethash (cons type id) (material-resource-texture-table material)) value))


(defmacro do-textures ((texture type id material) &body body)
  (with-gensyms (key)
    `(maphash (lambda (,key ,texture)
                (destructuring-bind (,type . ,id) ,key
                  ,@body))
              (material-resource-texture-table ,material))))
