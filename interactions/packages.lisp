(cl:defpackage :cl-bodge.interactions
  (:nicknames :ge.act)
  (:use :cl :cl-bodge.engine :cl-bodge.utils
        :cl-bodge.physics :cl-bodge.poiu :cl-bodge.graphics
        :cl-bodge.host :cl-bodge.scene)
  (:export interactions-system

           interactive-board-node
           make-board-window
           enable-mouse-input
           enable-cursor-input
           enable-keyboard-input
           enable-character-input))
