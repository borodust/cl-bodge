(in-package :cl-bodge.text)


(defclass glyph-atlas () ())


(defclass glyph () ())


(defclass text ()
  (text position))


(defmethod render ((this text))
  (with-slots (text position) this
    (loop for char across text do
         (render (find-glyph glyph atlas)))))
