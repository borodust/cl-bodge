(in-package :cl-bodge.assets)


(define-chunk-structure glyph-metrics
  character
  origin
  bounding-box
  advance-width
  kernings)


(define-chunk-structure (font-atlas-chunk t glyph-metrics)
  image-name
  ascender
  descender
  line-gap)


(defmethod parse-chunk ((chunk-type (eql :font-atlas)) params data)
  (make-font-atlas-chunk data))


(definline font-atlas-asset-id (font-name)
  (format nil "/sdf/~A/image" font-name))


(definline font-asset-id (font-name)
  (format nil "/sdf/~A/font" font-name))
