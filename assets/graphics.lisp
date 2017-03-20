(in-package :cl-bodge.assets)

;;;
;;;
;;;
(defenum pixel-format
  :grey :rgb :rgba)

;;;
;;;
;;;
(defgeneric pixel-format-of (image))
(defgeneric width-of (image))
(defgeneric height-of (image))
(defgeneric image->foreign-array (image))


;;;
;;;
;;;
(defgeneric shader-text-of (resource))
(defgeneric shader-type-of (resource))
(defgeneric shader-name-of (resource))
(defgeneric reload-shader-text (resource))
