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
(defgeneric size-of (image))
(defgeneric image->array (image))


;;;
;;;
;;;
(defgeneric shader-text-of (resource))
(defgeneric shader-type-of (resource))
(defgeneric shader-name-of (resource))
(defgeneric reload-shader-text (resource))
