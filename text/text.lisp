(in-package :cl-bodge.text)


(defclass text (disposable)
  ((position :initform nil)
   (text-mesh :initform nil)
   (width :initform nil :reader width-of)
   (height :initform nil :reader height-of)
   (atlas-tex :initform nil)))


(define-destructor text (text-mesh atlas-tex)
  (dispose text-mesh)
  (dispose atlas-tex))


(defmethod initialize-instance :after ((this text) &key text font)
  (with-slots (text-mesh atlas-tex width height) this
    (let* ((lines (split-sequence #\Newline text))
           (size (reduce #'+ lines :key #'length))
           (box-array (make-array (list size 4) :element-type 'single-float))
           (tex-coord-array (make-array (list size 4) :element-type 'single-float))
           (atlas (font-atlas-texture font))
           (line-height (+ (font-ascender-height font) (font-descender-height font)
                           (font-line-gap font))))
      (destructuring-bind (atlas-w atlas-h) (dimensions-of atlas)
        (setf text-mesh (make-mesh size :points)
              atlas-tex atlas)
        (loop with y = 0.0 and i = 0 and x-max = 0.0
           for line in lines
           for x = 0.0
           for prev-g = nil
           do (loop for c across line
                 for g = (find-glyph font c)
                 for (x0-box y0-box x1-box y1-box) = (glyph-bounding-box g)
                 for (x-orig y-orig) = (glyph-origin g)
                 for advance = (glyph-advance-width g)
                 do (let ((kerning (if prev-g (find-kerning prev-g g) 0)))
                      (setf (aref box-array i 0) #f(- x x-orig)
                            (aref box-array i 1) #f(- y y-orig)
                            (aref box-array i 2) #f(+ (aref box-array i 0) (- x1-box x0-box))
                            (aref box-array i 3) #f(+ (aref box-array i 1) (- y1-box y0-box))

                            (aref tex-coord-array i 0) #f(/ x0-box atlas-w)
                            (aref tex-coord-array i 1) #f(/ (- atlas-h y1-box) atlas-h)
                            (aref tex-coord-array i 2) #f(/ x1-box atlas-w)
                            (aref tex-coord-array i 3) #f(/ (- atlas-h y0-box) atlas-h)

                            x (+ x kerning advance)
                            prev-g g)
                      (incf i))
                 finally
                   (when (> x x-max) (setf x-max x))
                   (decf y line-height))
           finally (setf width x-max
                         height (abs y)))
        (with-disposable ((pbuf (make-array-buffer box-array))
                          (tbuf (make-array-buffer tex-coord-array)))
          (attach-array-buffer pbuf text-mesh 0)
          (attach-array-buffer tbuf text-mesh 1))))))


(defun make-text (string font)
  (make-instance 'text :text string :font font))


(defmethod render ((this text))
  (with-slots (text-mesh atlas-tex) this
    (with-bound-texture (atlas-tex)
      (render text-mesh))))
