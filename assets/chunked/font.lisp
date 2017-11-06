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


(defmethod write-chunk ((this font-atlas-chunk) stream)
  (let ((header (list (id-of this)
                      :image-name (font-atlas-chunk-image-name this)
                      :ascender (font-atlas-chunk-ascender this)
                      :descender (font-atlas-chunk-descender this)
                      :line-gap (font-atlas-chunk-line-gap this)))
        (glyphs (loop for glyph in (children-of this)
                   collect (list (id-of glyph)
                                 :character (glyph-metrics-character glyph)
                                 :bounding-box (glyph-metrics-bounding-box glyph)
                                 :origin (glyph-metrics-origin glyph)
                                 :advance-width (glyph-metrics-advance-width glyph)
                                 :kernings (glyph-metrics-kernings glyph))))
        (string-stream (flex:make-flexi-stream stream :external-format :utf-8))
        (*print-pretty* nil))
    (prin1 (cons header glyphs) string-stream)))
