(in-package :cl-bodge.animation)


(defclass skeleton () ())


(defclass bone (parent) ())


(defclass keyframe ()
  ((timestamp :initarg :timestamp :reader timestamp-of)
   (rotation :initarg :rotation :reader rotation-of)))


(defun make-keyframe (timestamp rotation)
  (make-instance 'keyframe :timestamp timestamp :rotation rotation))


(defclass keyframe-sequence ()
  ((frames :initform #() :initarg :frames :reader keyframes-of)))


(labels ((%timestamp (v) (timestamp-of v)))
  (defmethod initialize-instance ((this keyframe-sequence) &key sequence)
    (with-slots (frames) this
      (setf frames
            (make-array (length sequence)
                        :element-type 'keyframe
                        :initial-contents (sort sequence #'< :key #'%timestamp)))))


  (defun rotation-at (animation-sequence timestamp)
    (let ((kframes (keyframes-of animation-sequence)))
      (flet ((%interpolate (this-idx that-idx)
               (let* ((this (aref kframes this-idx))
                      (that (aref kframes that-idx))
                      (this-timestamp (timestamp-of this))
                      (f #f(/ (- timestamp this-timestamp)
                              (- (timestamp-of that) this-timestamp))))
                 (nlerp (rotation-of this) (rotation-of that) f))))
        (multiple-value-bind (frame idx) (search-sorted timestamp kframes :key #'%timestamp)
          (let* ((len (length kframes)))
            (if (null frame)
                (cond
                  ((= idx 0) (rotation-of (aref kframes 0)))
                  ((= idx len) (rotation-of (aref kframes (1- len))))
                  (t (%interpolate (1- idx) idx)))
                (rotation-of frame))))))))


(defun make-keyframe-sequence (frames)
  (make-instance 'keyframe-sequence :sequence frames))
