(in-package :cl-bodge.text)


(defgeneric font-atlas-texture (resource))
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


(defclass font (disposable)
  ((glyph-table :initform (make-hash-table :test 'equal))
   (atlas :initarg :atlas :reader font-atlas-texture)
   (ascender-height :initarg :ascender-height :reader font-ascender-height)
   (descender-height :initarg :descender-height :reader font-descender-height)
   (line-gap :initarg :line-gap :reader font-line-gap)))


(defmethod initialize-instance :after ((this font) &key glyphs)
  (with-slots (glyph-table) this
    (loop for g in glyphs
       do (setf (gethash (glyph-character g) glyph-table) g))))


(define-destructor font (atlas)
  (dispose atlas))


(defmethod find-glyph ((this font) character)
  (with-slots (glyph-table) this
    (gethash character glyph-table)))


(defun make-glyph (character origin bounding-box advance-width kernings)
  (make-instance 'glyph
                 :character character
                 :origin origin
                 :bounding-box bounding-box
                 :advance-width advance-width
                 :kernings kernings))


(define-system-function bake-font graphics-system (atlas-image glyphs ascender-height
                                                               descender-height
                                                               line-gap)
  (make-instance 'font
                 :glyphs glyphs
                 :atlas (make-2d-texture atlas-image :grey :generate-mipmaps-p nil)
                 :ascender-height ascender-height
                 :descender-height descender-height
                 :line-gap line-gap))


(defun walk-string (string font &optional walker)
  (let* ((line-height (+ (font-ascender-height font)
                         (font-descender-height font)
                         (font-line-gap font)))
         (len (length string))
         (atlas (font-atlas-texture font)))
    (destructuring-bind (atlas-w atlas-h) (dimensions-of atlas)
      (loop with y = 0.0 and x-max = 0.0 and idx = 0 and size = 0
         for next-idx = (or (position #\Newline string :start idx) len)
         for x = 0.0
         for prev-g = nil
         append (loop for i from idx below next-idx
                   for c = (aref string i)
                   for g = (find-glyph font c)
                   for (x0-box y0-box x1-box y1-box) = (glyph-bounding-box g)
                   for (x-orig y-orig) = (glyph-origin g)
                   for advance = (glyph-advance-width g)
                   collect (let ((kerning (if prev-g (find-kerning prev-g g) 0)))
                             (prog1
                                 (when walker
                                   (let ((x-offset (f (- x x-orig)))
                                         (y-offset (f (- y y-orig))))
                                     (funcall walker
                                              x-offset
                                              y-offset
                                              (f (+ x-offset (- x1-box x0-box)))
                                              (f (+ y-offset (- y1-box y0-box)))

                                              (f (/ x0-box atlas-w))
                                              (f (/ (- atlas-h y1-box) atlas-h))
                                              (f (/ x1-box atlas-w))
                                              (f (/ (- atlas-h y0-box) atlas-h)))))
                               (setf x (+ x kerning advance)
                                     prev-g g)))
                   into line-result
                   finally
                     (when (> x x-max) (setf x-max x))
                     (incf y line-height)
                     (incf size (- next-idx idx))
                     (setf idx (1+ next-idx))
                     (return line-result))
         into result
         until (= next-idx len)
         finally (return (values size x-max (abs y)))))))


(defun measure-string (string font)
  (multiple-value-bind (size width height) (walk-string string font)
    (declare (ignore size))
    (list width height)))
