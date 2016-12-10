(in-package :cl-bodge.interactions)



(defclass interactive-board-node (scene-node) ())


(defclass iboard-layout-node (interactive-board-node) ())


(defclass iboard-widget-layout (iboard-layout-node) ())


(defclass iboard-widget-node (interactive-board-node) ())


(defclass iboard-root-node (interactive-board-node) ())
