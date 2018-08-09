(cl:in-package :cl-bodge.resources)


(define-chunk-structure glyph-metrics-resource
  character
  origin
  bounding-box
  advance-width
  kernings)


(define-chunk-structure font-atlas-resource
  image-name
  ascender
  descender
  line-gap
  (glyphs :list glyph-metrics-resource))


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
  (:default-initargs :chunk-type 'font-atlas-resource
                     :resource-type :font-atlas))


(defmethod resource-dependencies ((this sdf-font-resource-handler) atlas)
  (list (font-atlas-resource-image-name atlas)))


(defmacro define-sdf-font (name)
  `(progn
     (defresource :font (sdf-font-metrics-resource-name ,name) :type :sdf)))
