(in-package :cl-bodge.assets)

;;;
;;; BRF
;;;
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
