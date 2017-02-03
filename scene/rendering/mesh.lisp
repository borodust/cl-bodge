(in-package :cl-bodge.scene)

;;;
;;;
;;;
(defclass mesh-node (scene-node)
  ((mesh :initform nil)))


(defgeneric make-node-mesh (node graphics-system))


(defmethod initialize-node :after ((this mesh-node) (sys graphics-system))
  (with-slots (mesh) this
    (setf mesh (make-node-mesh this sys))))


(defmethod node-enabled-p ((this mesh-node))
  (with-slots (mesh) this
    (not (null mesh))))


(defmethod scene-pass ((this mesh-node) (pass rendering-pass) input)
  (with-slots (mesh) this
    (render mesh)
    (call-next-method)))


(defmethod discard-node :before ((this mesh-node))
  (with-slots (mesh) this
    (let ((m mesh))
      (setf mesh nil)
      (dispose m))))
