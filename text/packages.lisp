(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.text
  (:nicknames :ge.text)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.graphics
        :cl-bodge.resources :cl-bodge.library.shading)
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
           text-line-height
           text-ascender-height
           update-text-renderer-canvas-size
           draw-text
           scale-of
           font-of))
