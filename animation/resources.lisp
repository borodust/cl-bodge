(in-package :cl-bodge.animation)


(defun parse-keyframe-animation (chunk)
  (unless (null chunk)
    (make-keyframe-animation
     (loop for seq in (ge.rsc:children-of chunk)
        for bone-id = (ge.rsc:keyframe-sequence-bone seq)
        unless (null bone-id)
        collect (cons bone-id
                      (make-keyframe-sequence
                       (loop for frame in (ge.rsc:children-of seq)
                          collect (destructuring-bind (timestamp rot transl scale) frame
                                    (make-keyframe timestamp
                                                   :rotation (sequence->quat rot)
                                                   :translation (sequence->vec3 transl)
                                                   :scale (sequence->vec3 scale))))))))))
