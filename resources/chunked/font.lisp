(cl:in-package :cl-bodge.resources)


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


(defmethod parse-chunk-structure ((chunk-type (eql :font-atlas)) data)
  (make-font-atlas-chunk data))


(defmethod serialize-chunk-structure ((this font-atlas-chunk))
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
                                 :kernings (glyph-metrics-kernings glyph)))))
    (cons header glyphs)))


;;;
;;; SDF font resource
;;;
(definline sdf-font-resource-name (name item)
  (fad:merge-pathnames-as-file (fad:pathname-as-directory name) item))


(definline sdf-font-atlas-resource-name (name)
  (sdf-font-resource-name name "image"))

(definline sdf-font-metrics-resource-name (name)
  (sdf-font-resource-name name "font"))

;;;
;;; SDF font resource handler
;;;
(defgeneric font-container-data (container))


(defclass sdf-font-resource-handler (chunk-structure-resource-handler) ()
  (:default-initargs :chunk-type :font-atlas))


(defmacro define-sdf-font (name)
  `(progn
     (defresource :image (sdf-font-atlas-resource-name ,name)
       :type :png)
     (defresource :font (sdf-font-metrics-resource-name ,name)
       :type :sdf)))
