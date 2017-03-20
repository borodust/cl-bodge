(in-package :cl-bodge.resources)


(defun chunk->animation (chunk)
  (unless (null chunk)
    (ge.ani:make-keyframe-animation
     (loop for seq in (children-of chunk)
        for bone = (keyframe-sequence-bone seq) unless (null bone) collect
          (cons (id-of bone)
                (ge.ani:make-keyframe-sequence
                 (loop for frame in (children-of seq) collect
                      (destructuring-bind (timestamp rot transl scale) frame
                        (ge.ani:make-keyframe timestamp
                                              :rotation (sequence->quat rot)
                                              :translation (sequence->vec3 transl)
                                              :scale (sequence->vec3 scale))))))))))


(defmethod chunk-asset-flow ((this animation-chunk) loader)
  (value-flow (chunk->animation this)))

;;;
;;;
;;;
(defstruct mesh-asset
  (mesh nil :read-only t)
  (transform nil :read-only t)
  (bones nil :read-only t))


(define-system-function chunk->mesh ge.gx:graphics-system (mesh-chunk)
  (let* ((arrays (mesh-chunk-arrays mesh-chunk))
         (v-count (length (cadar arrays)))
         (chunk-bones (mesh-chunk-bones mesh-chunk))
         (bone-count (reduce #'max chunk-bones :key #'mesh-bone-index :initial-value 0))
         (bones
          (loop with r = (make-array bone-count :initial-element nil)
             for bone in chunk-bones do
               (setf (aref r (1- (mesh-bone-index bone)))
                     (when-let ((skeleton-bone (mesh-bone-bone bone)))
                       (cons (id-of skeleton-bone)
                             (sequence->mat4 (mesh-bone-offset bone)))))
             finally (return r)))
         (index-array (list->array (mesh-chunk-indexes mesh-chunk)))
         (mesh (ge.gx:make-mesh v-count :triangles index-array)))
    (loop for (array-id array) in arrays do
         (with-disposable ((vbuf (ge.gx:make-array-buffer
                                  (list->array array v-count (length (car array))))))
           (ge.gx:attach-array-buffer vbuf mesh array-id)))
    (values mesh (sequence->mat4 (mesh-chunk-transform mesh-chunk)) bones)))


(defmethod chunk-asset-flow ((this mesh-chunk) loader)
  (-> ((ge.gx:graphics) :important-p t) ()
    (multiple-value-bind (mesh transform bones) (chunk->mesh this)
      (make-mesh-asset :mesh mesh
                       :transform transform
                       :bones bones))))


(defmethod dispose-chunk-asset ((this mesh-chunk) asset loader)
  (dispose (mesh-asset-mesh asset)))


(defun chunk->skeleton (chunk)
  (labels ((%traverse (bone)
             (let ((node (make-instance 'ge.sg:bone-node
                                        :name (id-of bone)
                                        :transform (sequence->mat4
                                                    (skeleton-bone-transform bone)))))
               (dolist (child (children-of bone))
                 (adopt node (%traverse child)))
               node)))
    (unless (null chunk)
      (%traverse chunk))))


(defmethod chunk-asset-flow ((this skeleton-bone) loader)
  (value-flow (chunk->skeleton this)))


(defun chunk->image (chunk)
  (let ((raw-data (image-chunk-data chunk)))
    (make-instance 'image
                   :width (image-chunk-width chunk)
                   :height (image-chunk-height chunk)
                   :pixel-format (image-chunk-pixel-format chunk)
                   :data (make-foreign-array (length raw-data)
                                             :initial-contents raw-data))))


(defmethod chunk-asset-flow ((this image-chunk) loader)
  (value-flow (chunk->image this)))


(define-system-function chunk->font ge.gx:graphics-system (font-chunk atlas-image)
  (let ((glyphs (loop for g in (children-of font-chunk)
                   collect (ge.text:make-glyph (code-char (glyph-metrics-character g))
                                               (glyph-metrics-origin g)
                                               (glyph-metrics-bounding-box g)
                                               (glyph-metrics-advance-width g)
                                               (glyph-metrics-kernings g)))))
    (ge.text:make-font atlas-image glyphs
                       (font-atlas-chunk-ascender font-chunk)
                       (- (font-atlas-chunk-descender font-chunk))
                       (font-atlas-chunk-line-gap font-chunk))))


(defmethod chunk-asset-flow ((this font-atlas-chunk) loader)
  (>> (load-asset loader (font-atlas-chunk-image-name this))
      (-> ((ge.gx:graphics) :important-p t) (image)
        (chunk->font this image))))


(defmethod dispose-chunk-asset ((this font-atlas-chunk) asset loader)
  (dispose asset))
