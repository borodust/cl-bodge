(bodge-util:define-package :cl-bodge.text
  (:nicknames :ge.text)
  (:use :cl :bodge-util :cl-bodge.engine :cl-bodge.graphics :cl-bodge.resources)
  (:export make-glyph
           build-sdf-font

           font-ascender-height
           font-descender-height
           font-line-gap

           text-pipeline

           measure-string
           make-text
           update-text
           render-text
           string-of
           width-of
           height-of

           make-text-renderer
           measure-scaled-string
           text-line-height
           text-ascender-height
           update-text-renderer-canvas-size
           print-text
           scale-of
           font-of))
