(in-package :cl-bodge.scene)


(defclass scenegraph-system (generic-system) ()
  (:default-initargs :depends-on '(graphics-system physics-system resource-system)))
