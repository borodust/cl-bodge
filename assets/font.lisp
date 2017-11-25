(in-package :cl-bodge.assets)

;;;
;;; SDF font resource
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
    (ge.text:bake-font atlas-image glyphs
                       (font-atlas-chunk-ascender font-chunk)
                       (- (font-atlas-chunk-descender font-chunk))
                       (font-atlas-chunk-line-gap font-chunk))))


;;;
;;; SDF font resource handler
;;;
(defclass sdf-font-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :font-atlas))


(defmacro define-sdf-font (name)
  `(progn
     (defresource :image (font-resource-name ,name "image")
       :type :png)
     (defresource :font (font-resource-name ,name "font")
       :type :sdf)))


;;;
;;; Conventional fonts
;;;
(defclass font-container ()
  ((data :initarg :data :reader ge.vg:font-container-data)))


(defclass truetype-font-resouce-handler () ())


(defmethod decode-resource ((this truetype-font-resouce-handler) stream)
  (make-instance 'font-container
                 :data (alexandria:read-stream-content-into-byte-vector stream)))


(defmethod encode-resource ((this truetype-font-resouce-handler) (value font-container) stream)
  (write-sequence (ge.vg:font-container-data value) stream))


;;;
;;; Font contstructor
;;;
(defmethod make-resource-handler ((type (eql :font)) &key ((:type font-type)
                                                           (error ":type missing")))
  (eswitch (font-type)
    (:sdf (make-instance 'sdf-font-resource-handler))
    (:ttf (make-instance 'truetype-font-resouce-handler))))
