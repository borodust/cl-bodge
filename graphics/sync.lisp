(cl:in-package :cl-bodge.graphics)


(defclass ping-pong-side ()
  ((object :initform nil :initarg :object :reader ping-pong-side-object)
   (fence :initform nil)
   (lock :initform nil)))


(defmethod initialize-instance :after ((this ping-pong-side) &key)
  (with-slots (lock) this
    (setf lock (bt:make-recursive-lock "ping-pong-side-lock"))))


(defun make-ping-pong-side (object)
  (make-instance 'ping-pong-side :object object))


(defun wait-for-side (side)
  (with-slots (fence lock) side
    (let ((fence (bt:with-recursive-lock-held (lock) fence)))
      (when fence
        (%gl:client-wait-sync fence :sync-flush-commands 1000000)))))


(defun refresh-side-fence (side)
  (with-slots (fence lock) side
    (bt:with-recursive-lock-held (lock)
      (when fence
        (%gl:delete-sync fence))
      (setf fence (%gl:fence-sync :sync-gpu-commands-complete 0)))))


(defmacro with-side-locked ((side) &body body)
  `(with-slots (lock) ,side
     (bt:with-recursive-lock-held (lock)
       (progn ,@body))))


(defclass ping-pong-pair ()
  ((front :initarg :front :reader ping-pong-pair-front)
   (back :initarg :back :reader ping-pong-pair-back)))


(defun make-ping-pong-pair (front back)
  (make-instance 'ping-pong-pair :front (make-ping-pong-side front)
                                 :back (make-ping-pong-side back)))


(defun ping-pong-swap (object)
  (with-slots (front back) object
    (with-side-locked (front)
      (with-side-locked (back)
        (wait-for-side front)
        (wait-for-side back)
        (let ((tmp front))
          (setf front back
                back tmp))))))


(defmacro with-ping-pong-side-locked ((side-var object accessor) &body body)
  (once-only (object accessor)
    `(with-side-locked ((funcall ,accessor ,object))
       (prog1 (let ((,side-var (ping-pong-side-object (funcall ,accessor ,object))))
                ,@body)
         (refresh-side-fence (funcall ,accessor ,object))))))


(defmacro with-ping-pong-front ((front-var object) &body body)
  `(with-ping-pong-side-locked (,front-var ,object #'ping-pong-pair-front)
     ,@body))


(defmacro with-ping-pong-back ((back-var object) &body body)
  `(with-ping-pong-side-locked (,back-var ,object #'ping-pong-pair-back)
     ,@body))
