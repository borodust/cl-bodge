(in-package :cl-bodge.assets)

;;;
;;; Font resource
;;;

(definline font-resource-name (name item)
  (fad:merge-pathnames-as-file (fad:pathname-as-directory name) item))


(define-system-function build-sdf-font ge.gx:graphics-system (name)
  (let* ((font-chunk (load-resource (font-resource-name name "font")))
         (atlas-image (load-resource (font-resource-name name "image")))
         (glyphs (loop for g in (children-of font-chunk)
                    collect (ge.text:make-glyph (code-char (glyph-metrics-character g))
                                                (glyph-metrics-origin g)
                                                (glyph-metrics-bounding-box g)
                                                (glyph-metrics-advance-width g)
                                                (glyph-metrics-kernings g)))))
    (ge.text:make-font atlas-image glyphs
                       (font-atlas-chunk-ascender font-chunk)
                       (- (font-atlas-chunk-descender font-chunk))
                       (font-atlas-chunk-line-gap font-chunk))))


;;;
;;; SDF font resource
;;;
(defclass sdf-font-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :font-atlas))


(defmethod make-resource-handler ((type (eql :font)) &key ((:type font-type)
                                                           (error ":type missing")))
  (eswitch (font-type)
    (:sdf (make-instance 'sdf-font-resource-handler))))


(defmacro define-sdf-font (name)
  `(progn
     (defresource :image (font-resource-name ,name "image")
       :type :png)
     (defresource :font (font-resource-name ,name "font")
       :type :sdf)))
