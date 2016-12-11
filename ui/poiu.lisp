(in-package :cl-bodge.interactions)


(defclass interactive-board-node (scene-node) ())

(defclass iboard-root (interactive-board-node) ())

(defclass iboard-layout (interactive-board-node) ())
(defclass iboard-widget-layout (interactive-board-node) ())

(defclass iboard-widget (interactive-board-node) ())
(defclass iboard-label (iboard-widget) ())
(defclass iboard-text-input (iboard-widget) ())
(defclass iboard-text (iboard-widget) ())
