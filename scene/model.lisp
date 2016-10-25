(in-package :cl-bodge.scene)


(defclass model (node) ())


(defgeneric make-model-graph (model))


(defmethod initialize-instance :after ((this model) &key)
  (adopt this (make-model-graph this)))
