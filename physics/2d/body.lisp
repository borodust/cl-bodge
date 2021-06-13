(cl:in-package :cl-bodge.physics.chipmunk)

;;;
;;; MASS
;;;
(defstruct mass
  (value 0f0 :type single-float :read-only t)
  (inertia 0f0 :type single-float :read-only t))


(defmethod simulation-engine-make-mass-for-circle ((engine chipmunk-engine) (mass number)
                                                   (radius number) &key (offset (vec2)))
  (let ((cp-mass (cp-float mass)))
    (with-cp-vect (vect offset)
      (make-mass :value cp-mass
                 :inertia (%chipmunk:moment-for-circle cp-mass (cp-float 0) (cp-float radius) vect)))))


(defmethod simulation-engine-make-mass-for-box ((engine chipmunk-engine) (mass number)
                                                (width number) (height number) &key (offset (vec2)))
  (let ((cp-mass (cp-float mass))
        (w/2 (/ width 2))
        (h/2 (/ height 2)))
    (c-with ((verts %chipmunk:vect :count 5))
      (init-cp-vect (verts 0) (vec2 (- w/2) (- h/2)))
      (init-cp-vect (verts 1) (vec2 (- w/2) h/2))
      (init-cp-vect (verts 2) (vec2 w/2 h/2))
      (init-cp-vect (verts 3) (vec2 w/2 (- h/2)))
      (init-cp-vect (verts 4) offset)
      (make-mass :value cp-mass
                 :inertia (%chipmunk:moment-for-poly cp-mass 4 (verts &) (verts 4 &) (cp-float 0))))))



;;;
;;; RIGID BODY
;;;
(defhandle rigid-body-handle
  :closeform (%chipmunk:body-free *handle-value*))

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
                        (%chipmunk:body-new-kinematic))
                      (%chipmunk:body-new (mass-value mass) (mass-inertia mass))))
         args))


(defmethod simulation-engine-make-rigid-body ((engine chipmunk-engine) (universe universe)
                                              &key mass kinematic)
  (let ((body (make-instance 'rigid-body :mass (or mass (and (not kinematic) (make-mass :value 1f0 :inertia 1f0)))
                                         :universe universe
                                         :kinematic kinematic)))
    (flet ((%space-add-body ()
             (%chipmunk:space-add-body (handle-value-of universe) (handle-value-of body))))
      (invoke-between-observations #'%space-add-body))
    body))


(defmethod simulation-engine-apply-force ((engine chipmunk-engine) (body rigid-body) (force vec2))
  (with-cp-vect (current-force)
    (%chipmunk:body-get-force current-force (handle-value-of body))
    (incf (current-force :x) (cp-float (x force)))
    (incf (current-force :y) (cp-float (y force)))
    (%chipmunk:body-set-force (handle-value-of body) current-force))
  force)


(defmethod simulation-engine-apply-torque ((engine chipmunk-engine) (body rigid-body) (torque number))
  (let* ((current-torque (%chipmunk:body-get-torque (handle-value-of body))))
    (%chipmunk:body-set-torque (handle-value-of body) (cp-float (+ current-torque torque))))
  torque)


(defmethod simulation-engine-body-mass ((engine chipmunk-engine) (body rigid-body))
  (let ((handle (handle-value-of body)))
    (make-mass :value (%chipmunk:body-get-mass handle)
               :inertia (%chipmunk:body-get-moment handle))))


(defmethod (setf simulation-engine-body-mass) ((value mass) (engine chipmunk-engine)
                                               (body rigid-body))
  (between-observations
    (let ((handle (handle-value-of body)))
      (%chipmunk:body-set-mass handle (cp-float (mass-value value)))
      (%chipmunk:body-set-moment handle (cp-float (mass-inertia value)))))
  value)


(defmethod simulation-engine-body-position ((engine chipmunk-engine) (body rigid-body))
  (with-cp-vect (vect)
    (%chipmunk:body-get-position vect (handle-value-of body))
    (init-bodge-vec (vec2) vect)))


(defmethod (setf simulation-engine-body-position) ((value vec2) (engine chipmunk-engine)
                                                   (body rigid-body))
  (between-observations
    (with-cp-vect (vect value)
      (%chipmunk:body-set-position (handle-value-of body) vect)))
  value)


(defmethod simulation-engine-body-linear-velocity ((engine chipmunk-engine) (body rigid-body))
  (with-cp-vect (vect)
    (%chipmunk:body-get-velocity vect (handle-value-of body))
    (init-bodge-vec (vec2) vect)))


(defmethod (setf simulation-engine-body-linear-velocity) ((value vec2) (engine chipmunk-engine)
                                                          (body rigid-body))
  (between-observations
    (with-cp-vect (vect value)
      (%chipmunk:body-set-velocity (handle-value-of body) vect)))
  value)


(defmethod simulation-engine-body-rotation ((engine chipmunk-engine) (body rigid-body))
  (euler-angle->mat2 (%chipmunk:body-get-angle (handle-value-of body))))


(defmethod (setf simulation-engine-body-rotation) ((value mat2)
                                                   (engine chipmunk-engine)
                                                   (body rigid-body))
  (between-observations
    (%chipmunk:body-set-angle (handle-value-of body) (cp-float (atan (mref value 1 0)
                                                               (mref value 0 0)))))
  value)


(defmethod simulation-engine-body-angular-velocity ((engine chipmunk-engine) (body rigid-body))
  (%chipmunk:body-get-angular-velocity (handle-value-of body)))


(defmethod (setf simulation-engine-body-angular-velocity) ((value number) (engine chipmunk-engine)
                                                           (body rigid-body))
  (between-observations
    (%chipmunk:body-set-angular-velocity (handle-value-of body) (cp-float value))))


(defmethod simulation-engine-destroy-rigid-body ((engine chipmunk-engine) (body rigid-body))
  (dispose body))
