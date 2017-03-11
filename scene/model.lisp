(in-package :cl-bodge.scene)


(defclass model (scene-node) ())


(defgeneric model-graph-assembly-flow (model))


(defmethod initialization-flow ((this model) &key)
  (>> (model-graph-assembly-flow this)
      (instantly (subgraph)
        (adopt this subgraph))))
