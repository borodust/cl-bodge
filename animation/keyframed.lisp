(in-package :cl-bodge.animation)

;;;
;;;
;;;

(defclass keyframe ()
  ((timestamp :initarg :timestamp :reader timestamp-of)
   (rotation :initarg :rotation :initform (identity-quat) :reader rotation-of)
   (translation :initarg :translation :initform (vec3) :reader translation-of)
   (scale :initarg :scale :initform (vec3 1.0 1.0 1.0) :reader scale-of)))


(defun make-keyframe (timestamp &key (rotation (identity-quat))
                                  (translation (vec3))
                                  (scale (vec3 1.0 1.0 1.0)))
  (make-instance 'keyframe :timestamp timestamp
                 :rotation rotation
                 :translation translation
                 :scale scale))


(defmethod transform-of ((this keyframe))
  (mult (vec->translation-mat4 (translation-of this))
        (quat->rotation-mat4 (rotation-of this))
        (vec->scaling-mat4 (scale-of this))))


(defmethod lerp ((this keyframe) (that keyframe) f)
  (make-instance 'keyframe
                 :timestamp (lerp (timestamp-of this) (timestamp-of that) f)
                 :rotation (nlerp (rotation-of this) (rotation-of that) f)
                 :translation (lerp (translation-of this) (translation-of that) f)
                 :scale (lerp (scale-of this) (scale-of that) f)))

;;;
;;;
;;;
(defclass keyframe-sequence ()
  ((frames :initform #() :initarg :frames :reader keyframes-of)))


(labels ((%timestamp (v) (timestamp-of v)))
  (defmethod initialize-instance ((this keyframe-sequence) &key sequence)
    (with-slots (frames) this
      (setf frames
            (make-array (length sequence)
                        :element-type 'keyframe
                        :initial-contents (sort sequence #'< :key #'%timestamp)))))


  (defun keyframe-at (animation-sequence timestamp &optional looped)
    (let* ((kframes (keyframes-of animation-sequence))
           (len (length kframes))
           (timestamp (if looped
                          (let ((last-timestamp (timestamp-of (aref kframes (1- len)))))
                            (if (/= last-timestamp 0.0)
                                (mod timestamp last-timestamp)
                                0.0))
                          timestamp)))
      (flet ((%interpolate (this-idx that-idx)
               (let* ((this (aref kframes this-idx))
                      (that (aref kframes that-idx))
                      (this-timestamp (timestamp-of this))
                      (f (f (/ (- timestamp this-timestamp)
                               (- (timestamp-of that) this-timestamp)))))
                 (lerp this that f))))
        (multiple-value-bind (frame idx) (search-sorted timestamp kframes :key #'%timestamp)
          (if (null frame)
              (cond
                ((= idx 0) (aref kframes 0))
                ((= idx len) (aref kframes (1- len)))
                (t (%interpolate (1- idx) idx)))
              frame)))))

  (defun transform-at (animation-sequence timestamp &optional looped)
    (transform-of (keyframe-at animation-sequence timestamp looped))))


(defun make-keyframe-sequence (frames)
  (make-instance 'keyframe-sequence :sequence frames))


;;
(defclass animation-frame ()
  ((keyframe-by-id :initarg :keyframe-by-id)))


(defun frame-key-of (frame id)
  (with-slots (keyframe-by-id) frame
    (gethash id keyframe-by-id)))


(defun frame-transform-of (frame id)
  (transform-of (frame-key-of frame id)))


(defmethod lerp ((this animation-frame) (that animation-frame) (factor number))
  (with-slots ((this-keyframes keyframe-by-id)) this
    (with-slots ((that-keyframes keyframe-by-id)) that
      (let ((keyframes (make-hash-table :test 'equal)))
        (loop for id being the hash-key of this-keyframes
           do (setf (gethash id keyframes)
                    (if-let ((that-keyframe (gethash id that-keyframes)))
                      (lerp (gethash id this-keyframes) that-keyframe factor)
                      (gethash id this-keyframes))))
        (loop for id being the hash-key of that-keyframes
           unless (gethash id keyframes)
           do (setf (gethash id keyframes) (gethash id that-keyframes)))
        (make-instance 'animation-frame :keyframe-by-id keyframes)))))


;;
(defclass keyframe-animation ()
  ((sequences :initarg :sequence-alist :initform (error "Keyframe sequences must be supplied"))))


(defun make-keyframe-animation (sequence-alist)
  (make-instance 'keyframe-animation :sequence-alist sequence-alist))


(defun frame-at (animation time &optional looped)
  (with-slots (sequences) animation
    (let ((keyframe-by-id (make-hash-table :test #'equal)))
      (loop for (id . seq) in sequences
         do (setf (gethash id keyframe-by-id) (keyframe-at seq time looped)))
      (make-instance 'animation-frame
                     :keyframe-by-id keyframe-by-id))))


;;
(defmacro keyframed (&body sequences)
  (labels ((parse-frame (frame)
             `(make-keyframe ,(first frame) :rotation (euler-angles->quat ,@(second frame))))
           (parse-seq (seq)
             `(cons ,(first seq)
                    (make-keyframe-sequence
                     (list ,@(loop for frame in (rest seq) collecting
                                  (parse-frame frame)))))))
    (let ((sequence-alist `(list ,@(loop for seq in sequences collecting
                                        (parse-seq seq)))))
      `(make-keyframe-animation ,sequence-alist))))
