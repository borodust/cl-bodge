(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.text
  (:nicknames :ge.text)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics :cl-bodge.assets)
  (:export make-glyph
           make-font
           font-ascender-height
           font-descender-height
           font-line-gap

           measure-string
           make-text
           render-text
           update-text
           string-of
           width-of
           height-of

           make-text-renderer
           measure-scaled-string
           draw-text
           scale-of
           font-of))
