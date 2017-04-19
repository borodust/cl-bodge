(in-package :cl-bodge.animation)


(defclass animation-channel ()
  ((started-at :initform (real-time-seconds))
   (active :initarg :initial-animation :initform (error ":initial-animation missing"))
   (next-at :initform nil)
   (next :initform nil)
   (overlap-interval :initform 0)))


(definline make-animation-channel (initial-animation)
  (make-instance 'animation-channel :initial-animation initial-animation))


(defun play-animation (channel animation &optional (overlap-interval 0))
  (with-slots (next next-at (this-overlap-interval overlap-interval)) channel
    (setf next-at (real-time-seconds)
          this-overlap-interval overlap-interval
          next animation)))


(defun current-frame-of (animation-channel)
  (with-slots (active next-at next overlap-interval started-at) animation-channel
    (let ((timestamp (real-time-seconds)))
      (if next
          (if (> timestamp (+ next-at overlap-interval))
              (prog1 (frame-at next (- timestamp next-at) t)
                (setf active next
                      started-at next-at
                      next nil))
              (lerp (frame-at active (- timestamp started-at) t)
                    (frame-at next (- timestamp next-at) t)
                    (/ (- timestamp next-at)
                       overlap-interval)))
          (frame-at active (- timestamp started-at) t)))))
