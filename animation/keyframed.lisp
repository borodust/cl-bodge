(cl:in-package :cl-bodge.animation)

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


(defgeneric duration-of (sequence)
  (:method ((this keyframe-sequence))
    (with-slots (frames) this
      (timestamp-of (aref frames (1- (length frames)))))))


(defmethod initialize-instance ((this keyframe-sequence) &key sequence)
  (with-slots (frames) this
    (setf frames
          (make-array (length sequence)
                      :element-type 'keyframe
                      :initial-contents (sort sequence #'< :key #'timestamp-of)))))


(defun keyframe-at (animation-sequence timestamp)
  (let* ((kframes (keyframes-of animation-sequence))
         (len (length kframes)))
    (flet ((%interpolate (this-idx that-idx)
             (let* ((this (aref kframes this-idx))
                    (that (aref kframes that-idx))
                    (this-timestamp (timestamp-of this))
                    (f (f (/ (- timestamp this-timestamp)
                             (- (timestamp-of that) this-timestamp)))))
               (lerp this that f))))
      (multiple-value-bind (frame idx) (search-sorted timestamp kframes :key #'timestamp-of)
        (if (null frame)
            (cond
              ((= idx 0) (aref kframes 0))
              ((= idx len) (aref kframes (1- len)))
              (t (%interpolate (1- idx) idx)))
            frame)))))

(defun transform-at (animation-sequence timestamp)
  (transform-of (keyframe-at animation-sequence timestamp)))


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
(defgeneric frame-at (animation time))
(defclass keyframe-animation ()
  ((sequences :initarg :sequence-alist :initform (error "Keyframe sequences must be supplied"))
   (duration :reader duration-of)))


(defmethod initialize-instance :after ((this keyframe-animation) &key)
  (with-slots (sequences duration) this
    (flet ((%duration-of (pair)
             (duration-of (cdr pair))))
      (setf duration (reduce #'max sequences :key #'%duration-of :initial-value 0)))))


(defun make-keyframe-animation (sequence-alist)
  (make-instance 'keyframe-animation :sequence-alist sequence-alist))


(defmethod frame-at ((animation keyframe-animation) time)
  (with-slots (sequences duration) animation
    (let ((keyframe-by-id (make-hash-table :test #'equal)))
      (loop for (id . seq) in sequences
         do (setf (gethash id keyframe-by-id) (keyframe-at seq time)))
      (make-instance 'animation-frame
                     :keyframe-by-id keyframe-by-id))))


;;
(defclass decorator ()
  ((delegate :initarg :delegate :initform (error ":delegate missing") :reader delegate-of)))


(defmethod duration-of ((this decorator))
  (duration-of (delegate-of this)))


(defmethod frame-at ((this decorator) time)
  (frame-at (delegate-of this) time))


;;
(defclass looped-animation (decorator) ())


(defun make-looped-animation (source)
  (make-instance 'looped-animation
                 :delegate (or source (error "Source animation missing"))))


(defmethod frame-at ((this looped-animation) time)
  (let* ((source (delegate-of this))
         (duration (duration-of source))
         (looped-time (if (= duration 0.0) 0.0 (mod time duration))))
    (frame-at source looped-time)))


;;
(defclass inverted-animation (decorator) ())


(defun make-inverted-animation (source)
  (make-instance 'inverted-animation :delegate source))


(defmethod frame-at ((this inverted-animation) time)
  (let* ((source (delegate-of this))
         (duration (duration-of source))
         (inverted-time (- duration time)))
    (frame-at source inverted-time)))


;;
(defclass blended-animation (decorator)
  ((other :initarg :other :initform (error ":other missing"))
   (factor :initform 0.5 :initarg :factor)))


(defun make-blended-animation (this that &optional (factor 0.5))
  (make-instance 'blended-animation
                 :delegate this
                 :other that
                 :factor factor))


(defmethod frame-at ((this blended-animation) time)
  (with-slots (other factor) this
    (lerp (frame-at (delegate-of this) time)
          (frame-at other time)
          factor)))

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
