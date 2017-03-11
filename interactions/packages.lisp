(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.interactions
  (:nicknames :ge.act)
  (:use :cl :cl-bodge.engine :cl-bodge.utils
        :cl-bodge.physics :cl-bodge.poiu :cl-bodge.graphics :cl-bodge.event
        :cl-bodge.host :cl-bodge.scene :cl-bodge.assets)
  (:export interactions-system
           make-board-window
           interactive-board-node))
