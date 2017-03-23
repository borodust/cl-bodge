(in-package :cl-bodge.scene)


(defclass model (scene-node)
  ((model-root)))


(defgeneric model-graph-assembly-flow (model))


(defmethod initialization-flow ((this model) &key)
  (with-slots (model-root) this
    (>> (call-next-method)
        (model-graph-assembly-flow this)
        (instantly (subgraph)
          (setf model-root subgraph)))))


(defmethod scene-pass ((this model) pass input)
  (with-slots (model-root) this
    (scene-pass model-root pass input))
  (call-next-method))
