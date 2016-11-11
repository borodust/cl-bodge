(in-package :cl-bodge.scene)


(defun chunk->animation (chunk)
  (unless (null chunk)
    (make-keyframe-animation
     (loop for seq in (animation-chunk-children chunk)
        for bone = (keyframe-sequence-bone seq) unless (null bone) collect
          (cons (skeleton-bone-id bone)
                (make-keyframe-sequence
                 (loop for frame in (keyframe-sequence-children seq) collect
                      (destructuring-bind (timestamp rot transl scale) frame
                        (make-keyframe timestamp
                                       :rotation (sequence->quat rot)
                                       :translation (sequence->vec3 transl)
                                       :scale (sequence->vec3 scale))))))))))


(defun chunk->skeleton (chunk)
  (labels ((%traverse (bone)
             (let ((node (make-instance 'bone-node
                                        :name (skeleton-bone-id bone)
                                        :transform (sequence->mat4
                                                    (skeleton-bone-transform bone)))))
               (dolist (child (skeleton-bone-children bone))
                 (adopt node (%traverse child)))
               node)))
    (unless (null chunk)
      (%traverse chunk))))
