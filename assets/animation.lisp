(in-package :cl-bodge.assets)


(defun chunk->animation (chunk)
  (unless (null chunk)
    (ge.ani:make-keyframe-animation
     (loop for seq in (children-of chunk)
        for bone-id = (keyframe-sequence-bone seq)
        unless (null bone-id)
        collect (cons bone-id
                      (ge.ani:make-keyframe-sequence
                       (loop for frame in (children-of seq)
                          collect (destructuring-bind (timestamp rot transl scale) frame
                                    (ge.ani:make-keyframe timestamp
                                                          :rotation (sequence->quat rot)
                                                          :translation (sequence->vec3 transl)
                                                          :scale (sequence->vec3 scale))))))))))

;;;
;;; Animation resource
;;;
(defclass animation-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :animation))


(defmethod convert-from-chunk ((this animation-resource-handler) chunk)
  (chunk->animation chunk))


(defmethod make-resource-handler ((type (eql :animation)) &key)
  (make-instance 'animation-resource-handler))
