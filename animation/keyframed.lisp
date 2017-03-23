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


(defgeneric transform-of (frame)
  (:method ((this keyframe))
    (mult (vec->translation-mat4 (translation-of this))
          (quat->rotation-mat4 (rotation-of this))
          (vec->scaling-mat4 (scale-of this)))))


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


  (defun transform-at (animation-sequence timestamp)
    (let ((kframes (keyframes-of animation-sequence)))
      (flet ((%interpolate (this-idx that-idx)
               (let* ((this (aref kframes this-idx))
                      (that (aref kframes that-idx))
                      (this-timestamp (timestamp-of this))
                      (f (f (/ (- timestamp this-timestamp)
                               (- (timestamp-of that) this-timestamp)))))
                 (transform-of (lerp this that f)))))
        (multiple-value-bind (frame idx) (search-sorted timestamp kframes :key #'%timestamp)
          (let* ((len (length kframes)))
            (if (null frame)
                (cond
                  ((= idx 0) (transform-of (aref kframes 0)))
                  ((= idx len) (transform-of (aref kframes (1- len))))
                  (t (%interpolate (1- idx) idx)))
                (transform-of frame))))))))


(defun make-keyframe-sequence (frames)
  (make-instance 'keyframe-sequence :sequence frames))


(defclass keyframe-animation ()
  ((sequences :initarg :sequence-alist :initform (error "Keyframe sequences must be supplied"))
   (started-at :initform nil)))


(defun make-keyframe-animation (sequence-alist)
  (make-instance 'keyframe-animation :sequence-alist sequence-alist))


(defun frame-at (animation &optional (timestamp (local-time:now)))
  (with-slots (sequences started-at) animation
    (unless (null started-at)
      (let ((delta (max (- (epoch-seconds timestamp) started-at) 0.0)))
        (loop for (id . seq) in sequences collecting
             (cons id (transform-at seq delta)))))))


(defun frame-transform-of (frame id)
  (cdr (assoc id frame :test #'equal)))


(defun start-animation (animation)
  (with-slots (started-at) animation
    (setf started-at (epoch-seconds))))


(defun reset-animation (animation)
  (with-slots (started-at) animation
    (setf started-at nil)))


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
