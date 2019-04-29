(cl:in-package :cl-bodge.physics.chipmunk)

;;;
;;; MASS
;;;
(defstruct mass
  (value 0d0 :type double-float :read-only t)
  (inertia 0d0 :type double-float :read-only t))


(defmethod simulation-engine-make-mass-for-circle ((engine chipmunk-engine) (mass number)
                                                   (radius number) &key (offset (vec2)))
  (let ((cp-mass (cp-float mass)))
    (with-cp-vect (vect offset)
      (make-mass :value cp-mass
                 :inertia (%cp:moment-for-circle cp-mass (cp-float 0) (cp-float radius) vect)))))


(defmethod simulation-engine-make-mass-for-box ((engine chipmunk-engine) (mass number)
                                                (width number) (height number) &key (offset (vec2)))
  (let ((cp-mass (cp-float mass))
        (w/2 (/ width 2))
        (h/2 (/ height 2)))
    (claw:c-with ((verts %cp:vect :count 5))
      (init-cp-vect (verts 0) (vec2 (- w/2) (- h/2)))
      (init-cp-vect (verts 1) (vec2 (- w/2) h/2))
      (init-cp-vect (verts 2) (vec2 w/2 h/2))
      (init-cp-vect (verts 3) (vec2 w/2 (- h/2)))
      (init-cp-vect (verts 4) offset)
      (make-mass :value cp-mass
                 :inertia (%cp:moment-for-poly cp-mass 4 (verts &) (verts 4 &) (cp-float 0))))))



;;;
;;; RIGID BODY
;;;
(defhandle rigid-body-handle
  :closeform (%cp:body-free *handle-value*))

(defclass rigid-body (disposable)
  ((universe :initarg :universe :initform (error ":universe missing"))
   (handle :initarg :handle :initform (error ":handle missing") :reader handle-of)))


(define-destructor rigid-body (universe (body-handle handle-of))
  (%remove-and-free-body universe body-handle))


(defmethod initialize-instance ((this rigid-body) &rest args &key mass kinematic &allow-other-keys)
  (apply #'call-next-method this
         :handle (make-rigid-body-handle
                  (if kinematic
                      (progn
                        (when mass (error "Kinematic bodies cannot have mass"))
                        (%cp:body-new-kinematic))
                      (%cp:body-new (mass-value mass) (mass-inertia mass))))
         args))


(defmethod simulation-engine-make-rigid-body ((engine chipmunk-engine) (universe universe)
                                              &key mass kinematic)
  (let ((body (make-instance 'rigid-body :mass (or mass (and (not kinematic) (make-mass :value 1d0 :inertia 1d0)))
                                         :universe universe
                                         :kinematic kinematic)))
    (%cp:space-add-body (handle-value-of universe) (handle-value-of body))
    body))


(defmethod simulation-engine-apply-force ((engine chipmunk-engine) (body rigid-body) (force vec2))
  (with-cp-vect (current-force)
    (%cp:body-get-force current-force (handle-value-of body))
    (incf (current-force :x) (cp-float (x force)))
    (incf (current-force :y) (cp-float (y force)))
    (%cp:body-set-force (handle-value-of body) current-force))
  force)


(defmethod simulation-engine-apply-torque ((engine chipmunk-engine) (body rigid-body) (torque number))
  (let* ((current-torque (%cp:body-get-torque (handle-value-of body))))
    (%cp:body-set-torque (handle-value-of body) (cp-float (+ current-torque torque))))
  torque)


(defmethod simulation-engine-body-mass ((engine chipmunk-engine) (body rigid-body))
  (let ((handle (handle-value-of body)))
    (make-mass :value (%cp:body-get-mass handle)
               :inertia (%cp:body-get-moment handle))))


(defmethod (setf simulation-engine-body-mass) ((value mass) (engine chipmunk-engine)
                                               (body rigid-body))
  (let ((handle (handle-value-of body)))
    (%cp:body-set-mass handle (cp-float (mass-value value)))
    (%cp:body-set-moment handle (cp-float (mass-inertia value))))
  value)


(defmethod simulation-engine-body-position ((engine chipmunk-engine) (body rigid-body))
  (with-cp-vect (vect)
    (%cp:body-get-position vect (handle-value-of body))
    (init-bodge-vec (vec2) vect)))


(defmethod (setf simulation-engine-body-position) ((value vec2) (engine chipmunk-engine)
                                                   (body rigid-body))
  (with-cp-vect (vect value)
    (%cp:body-set-position (handle-value-of body) vect))
  value)


(defmethod simulation-engine-body-linear-velocity ((engine chipmunk-engine) (body rigid-body))
  (with-cp-vect (vect)
    (%cp:body-get-velocity vect (handle-value-of body))
    (init-bodge-vec (vec2) vect)))


(defmethod (setf simulation-engine-body-linear-velocity) ((value vec2) (engine chipmunk-engine)
                                                          (body rigid-body))
  (with-cp-vect (vect value)
    (%cp:body-set-velocity (handle-value-of body) vect))
  value)


(defmethod simulation-engine-body-rotation ((engine chipmunk-engine) (body rigid-body))
  (euler-angle->mat2 (%cp:body-get-angle (handle-value-of body))))


(defmethod (setf simulation-engine-body-rotation) ((value mat2)
                                                   (engine chipmunk-engine)
                                                   (body rigid-body))
  (%cp:body-set-angle (handle-value-of body) (cp-float (atan (mref value 1 0)
                                                             (mref value 0 0))))
  value)


(defmethod simulation-engine-body-angular-velocity ((engine chipmunk-engine) (body rigid-body))
  (%cp:body-get-angular-velocity (handle-value-of body)))


(defmethod (setf simulation-engine-body-angular-velocity) ((value number) (engine chipmunk-engine)
                                                           (body rigid-body))
  (%cp:body-set-angular-velocity (handle-value-of body) (cp-float value)))


(defmethod simulation-engine-destroy-rigid-body ((engine chipmunk-engine) (body rigid-body))
  (dispose body))
