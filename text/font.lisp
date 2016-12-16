(in-package :cl-bodge.text)


(defgeneric font-atlas (resource))
(defgeneric font-ascender-height (resource))
(defgeneric font-descender-height (resource))
(defgeneric font-line-gap (resource))
(defgeneric find-glyph (resource character))
(defgeneric find-kerning (this-glyph that-glyph))

(defgeneric glyph-character (resource))
(defgeneric glyph-origin (resource))
(defgeneric glyph-bounding-box (resource))
(defgeneric glyph-advance-width (resource))


(defclass glyph ()
  ((character :initarg :character :reader glyph-character)
   (origin :initarg :origin :reader glyph-origin)
   (bounding-box :initarg :bounding-box :reader glyph-bounding-box)
   (advance-width :initarg :advance-width :reader glyph-advance-width)
   (kerning-table :initform nil)))


(defmethod initialize-instance :after ((this glyph) &key kernings)
  (setf (slot-value this 'kerning-table) (alist-hash-table kernings :test 'equal)))


(defmethod find-kerning ((this glyph) (that character))
  (with-slots (kerning-table) this
    (gethash that kerning-table 0)))


(defmethod find-kerning ((this glyph) (that glyph))
  (find-kerning this (glyph-character that)))


(defclass font ()
  ((glyph-table :initform (make-hash-table :test 'equal))
   (atlas :initarg :atlas :reader font-atlas)
   (ascender-height :initarg :ascender-height :reader font-ascender-height)
   (descender-height :initarg :descender-height :reader font-descender-height)
   (line-gap :initarg :line-gap :reader font-line-gap)))


(defmethod initialize-instance :after ((this font) &key glyphs)
  (with-slots (glyph-table) this
    (loop for g in glyphs
       do (setf (gethash (glyph-character g) glyph-table) g))))


(defmethod find-glyph ((this font) character)
  (with-slots (glyph-table) this
    (gethash character glyph-table)))


(defun font-atlas-chunk->font (font-chunk atlas-image)
  (let ((glyphs (loop for g in (font-atlas-chunk-children font-chunk)
                   collect (make-instance 'glyph
                                          :character (glyph-metrics-character g)
                                          :origin (glyph-metrics-origin g)
                                          :bounding-box (glyph-metrics-bounding-box g)
                                          :advance-width (glyph-metrics-advance-width g)
                                          :kernings (glyph-metrics-kernings g)))))
    (make-instance 'font
                   :glyphs glyphs
                   :atlas atlas-image
                   :ascender-height (font-atlas-chunk-ascender font-chunk)
                   :descender-height (- (font-atlas-chunk-descender font-chunk))
                   :line-gap (font-atlas-chunk-line-gap font-chunk))))
