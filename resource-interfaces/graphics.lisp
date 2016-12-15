(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.graphics.resources
  (:nicknames :ge.gx.rsc)
  (:use :cl-bodge.utils
        :cl)
  (:export pixel-format
           pixel-format-p

           pixel-format-of
           image->array
           size-of

           shader-text-of
           shader-type-of
           shader-name-of
           reload-shader-text))


(in-package :cl-bodge.graphics.resources)

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
