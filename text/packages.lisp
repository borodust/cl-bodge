(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.text
  (:nicknames :ge.txt)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics :cl-bodge.assets
        :cl-bodge.graphics.resources :cl-bodge.resources)
  (:export font-atlas-chunk->font
           font-ascender-height
           font-descender-height
           font-line-gap

           measure-string
           make-text
           width-of
           height-of))
