(cl:in-package :cl-bodge.resources)


(defclass scene ()
  ((mesh-table :initform (make-hash-table) :reader %mesh-table-of)))


(defun make-empty-scene ()
  (make-instance 'scene))


(defun scene-mesh (scene id)
  (with-slots (mesh-table) scene
    (gethash id mesh-table)))


(defun (setf scene-mesh) (mesh scene id)
  (with-slots (mesh-table) scene
    (setf (gethash id mesh-table) mesh)))


(defclass mesh ()
  ((primitive :initarg :primitive :initform (error ":primitive missing"))
   (position-array :initform nil)
   (index-array :initform nil)
   (normal-array :initform nil)))


(defun make-mesh (primitive)
  (make-instance 'mesh :primitive primitive))


(defun mesh-primitive (mesh)
  (with-slots (primitive) mesh
    primitive))


(defun (setf mesh-position-array) (value mesh)
  (with-slots (position-array) mesh
    (setf position-array value)))


(defun mesh-position-array (mesh)
  (with-slots (position-array) mesh
    position-array))


(defun (setf mesh-index-array) (value mesh)
  (with-slots (index-array) mesh
    (setf index-array value)))


(defun mesh-index-array (mesh)
  (with-slots (index-array) mesh
    index-array))


(defun (setf mesh-normal-array) (value mesh)
  (with-slots (normal-array) mesh
    (setf normal-array value)))


(defun mesh-normal-array (mesh)
  (with-slots (normal-array) mesh
    normal-array))


(defmacro do-meshes ((mesh id scene) &body body)
  `(maphash (lambda (,id ,mesh) ,@body) (%mesh-table-of ,scene)))
