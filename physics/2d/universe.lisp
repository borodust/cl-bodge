(cl:in-package :cl-bodge.physics.chipmunk)


(declaim (special *active-universe*))


(defhandle universe-handle
  :initform (claw:with-float-traps-masked ()
              (%cp:space-new))
  :closeform (%cp:space-free *handle-value*))


(defclass universe (foreign-object)
  ((shape-registry :initform (trivial-garbage:make-weak-hash-table :weakness :value :test 'eql))
   (on-pre-solve :initform nil :initarg :on-pre-solve)
   (on-post-solve :initform nil :initarg :on-post-solve)
   (ptr-store :initform (claw:alloc :pointer 2))
   (post-step-queue :initform (make-task-queue)))
  (:default-initargs :handle (make-universe-handle)))


(define-destructor universe (ptr-store)
  (claw:free ptr-store))


(defun register-shape (universe id shape)
  (with-slots (shape-registry) universe
    (setf (gethash id shape-registry) shape)))


(definline find-shape (universe shape-id)
  (with-slots (shape-registry) universe
    (gethash shape-id shape-registry)))


(defun universe-locked-p (universe)
  (= %cp:+true+ (%cp:space-is-locked (handle-value-of universe))))


(defun %remove-and-free-shape (universe shape-handle)
  (with-slots (post-step-queue) universe
    (flet ((%destroy-shape ()
             (%cp:space-remove-shape (handle-value-of universe) (value-of shape-handle))
             (destroy-handle shape-handle)))
    (if (universe-locked-p universe)
        (push-task  #'%destroy-shape post-step-queue)
        (%destroy-shape)))))


(defun %remove-and-free-body (universe body-handle)
  (with-slots (post-step-queue) universe
    (flet ((%destroy-body ()
             (%cp:space-remove-body (handle-value-of universe) (value-of body-handle))
             (destroy-handle body-handle)))
    (if (universe-locked-p universe)
        (push-task  #'%destroy-body post-step-queue)
        (%destroy-body)))))


(defmacro with-colliding-shapes ((this that) arbiter &body body)
  (with-gensyms (store vec)
    (once-only (arbiter)
      `(with-slots ((,store ptr-store)) *active-universe*
         (claw:c-let ((,vec :pointer :ptr ,store))
           (%cp:arbiter-get-shapes ,arbiter (,vec 0 &) (,vec 1 &))
           (let ((,this (find-shape *active-universe* (cffi:pointer-address (,vec 0))))
                 (,that (find-shape *active-universe* (cffi:pointer-address (,vec 1)))))
             ,@body))))))


(claw:defcallback pre-solve-callback :int ((arbiter :pointer) (space :pointer) (data :pointer))
  (declare (ignore space data))
  (with-slots (on-pre-solve ptr-store) *active-universe*
    (if-let (pre-solve-fu on-pre-solve)
      (with-colliding-shapes (this that) arbiter
        (let ((*arbiter* arbiter))
          (if (funcall pre-solve-fu this that) %cp:+true+ %cp:+false+)))
      %cp:+true+)))


(claw:defcallback post-solve-callback :void ((arbiter :pointer) (space :pointer) (data :pointer))
  (declare (ignore space data))
  (with-slots (on-post-solve) *active-universe*
    (when-let (post-solve-fu on-post-solve)
      (with-colliding-shapes (this that) arbiter
        (let ((*arbiter* arbiter))
          (funcall post-solve-fu this that))))))


(defmethod initialize-instance :after ((this universe) &key)
  (claw:c-let ((collision-handler %cp:collision-handler
                                  :from (%cp:space-add-default-collision-handler
                                         (handle-value-of this))))
    (setf (collision-handler :pre-solve-func) (claw:callback 'pre-solve-callback)
          (collision-handler :post-solve-func) (claw:callback 'post-solve-callback))))


(defmethod simulation-engine-make-universe ((this chipmunk-engine)
                                            &key on-pre-solve on-post-solve)
  (make-instance 'universe :on-pre-solve on-pre-solve :on-post-solve on-post-solve))


(defmethod simulation-engine-destroy-universe ((this chipmunk-engine) (universe universe))
  (dispose universe))


(defun universe-static-body (universe)
  (%cp:space-get-static-body (handle-value-of universe)))


(defmethod (setf simulation-engine-gravity) ((value vec2) (this chipmunk-engine) (universe universe))
  (with-cp-vect (vec value)
    (%cp:space-set-gravity (handle-value-of universe) vec))
  value)


(defmethod simulation-engine-gravity ((this chipmunk-engine) (universe universe))
  (with-cp-vect (vec)
    (%cp:space-get-gravity vec (handle-value-of universe))
    (init-bodge-vec (vec2) vec)))


(defmethod simulation-engine-observe-universe ((engine chipmunk-engine) (universe universe) time-step)
  (with-slots (post-step-queue) universe
    (let ((*active-universe* universe))
      (claw:with-float-traps-masked ()
        (%cp:space-step (handle-value-of universe) (cp-float time-step))
        (drain post-step-queue)))))
