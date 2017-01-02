(in-package :cl-bodge.resources)


(defun chunk->animation (chunk)
  (unless (null chunk)
    (ge.ani:make-keyframe-animation
     (loop for seq in (animation-chunk-children chunk)
        for bone = (keyframe-sequence-bone seq) unless (null bone) collect
          (cons (skeleton-bone-id bone)
                (ge.ani:make-keyframe-sequence
                 (loop for frame in (keyframe-sequence-children seq) collect
                      (destructuring-bind (timestamp rot transl scale) frame
                        (ge.ani:make-keyframe timestamp
                                              :rotation (sequence->quat rot)
                                              :translation (sequence->vec3 transl)
                                              :scale (sequence->vec3 scale))))))))))


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
                       (cons (skeleton-bone-id skeleton-bone)
                             (sequence->mat4 (mesh-bone-offset bone)))))
             finally (return r)))
         (index-array (list->array (mesh-chunk-indexes mesh-chunk)))
         (mesh (ge.gx:make-mesh v-count :triangles index-array)))
    (loop for (array-id array) in arrays do
         (with-disposable ((vbuf (ge.gx:make-array-buffer
                                  (list->array array v-count (length (car array))))))
           (ge.gx:attach-array-buffer vbuf mesh array-id)))
    (values mesh (sequence->mat4 (mesh-chunk-transform mesh-chunk)) bones)))


(defun chunk->skeleton (chunk)
  (labels ((%traverse (bone)
             (let ((node (make-instance 'ge.sg:bone-node
                                        :name (skeleton-bone-id bone)
                                        :transform (sequence->mat4
                                                    (skeleton-bone-transform bone)))))
               (dolist (child (skeleton-bone-children bone))
                 (adopt node (%traverse child)))
               node)))
    (unless (null chunk)
      (%traverse chunk))))


(defun chunk->image (chunk)
  (make-instance 'image
                 :width (image-chunk-width chunk)
                 :height (image-chunk-height chunk)
                 :pixel-format (image-chunk-pixel-format chunk)
                 :data (image-chunk-data chunk)))


(define-system-function chunk->font ge.gx:graphics-system (font-chunk atlas-image)
  (let ((glyphs (loop for g in (font-atlas-chunk-children font-chunk)
                   collect (ge.txt:make-glyph (code-char (glyph-metrics-character g))
                                              (glyph-metrics-origin g)
                                              (glyph-metrics-bounding-box g)
                                              (glyph-metrics-advance-width g)
                                              (glyph-metrics-kernings g)))))
    (ge.txt:make-font atlas-image glyphs
                      (font-atlas-chunk-ascender font-chunk)
                      (- (font-atlas-chunk-descender font-chunk))
                      (font-atlas-chunk-line-gap font-chunk))))
