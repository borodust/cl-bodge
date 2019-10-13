(cl:in-package :cl-bodge.physics.ode)


(defparameter *world-quick-step-iterations* 40)
(defparameter *auto-disable-bodies-p* t)
(defparameter *error-reduction-parameter* 0.1f0) ;; 0.1 ~ 0.8 recommended
(defparameter *constant-force-mixing* 0.00001f0)   ;; 10e-9 ~ 1.0 recommended
(defparameter *contact-points-per-collision* 3)
;; https://en.wikipedia.org/wiki/Successive_over-relaxation
(defparameter *world-over-relaxation* 1.3f0)


(defclass universe ()
  ((world :initform (%ode:world-create) :reader world-of)
   (space :initform (%ode:hash-space-create (cffi:null-pointer)) :reader space-of)
   (geoms :initform (tg:make-weak-hash-table :weakness :value :test 'eql) :reader geoms-of)
   (pre-solve :initarg :pre-solve :initform nil :reader pre-solve-of)
   (post-solve :initarg :post-solve :initform nil :reader post-solve-of)))


(defun %register-geom (universe geom)
  (with-slots (geoms) universe
    (let ((ptr-addr (cffi:pointer-address (handle-value-of geom))))
      (setf (gethash ptr-addr geoms) geom))))


(defmethod initialize-instance :after ((this universe) &key)
  (with-slots (world) this
    (%ode:world-set-quick-step-num-iterations world *world-quick-step-iterations*)
    (%ode:world-set-quick-step-w world *world-over-relaxation*)
    (%ode:world-set-auto-disable-flag world (if *auto-disable-bodies-p* 1 0))
    (%ode:world-set-erp world (ode-real *error-reduction-parameter*))
    (%ode:world-set-cfm world (ode-real *constant-force-mixing*))))


(defun make-universe (on-pre-solve on-post-solve)
  (make-instance 'universe :pre-solve on-pre-solve :post-solve on-post-solve))


(defun destroy-universe (uni)
  (with-slots (world space) uni
    (%ode:world-destroy world)
    (%ode:space-destroy space)))


(defun %filter-contacts (contact-count contact-geoms)
  (loop for i from 0 below contact-count
        collect (make-contact (c-ref contact-geoms %ode:contact-geom i)
                              (collision-friction *collision*)
                              (collision-elasticity *collision*)
                              (collision-surface-velocity *collision*))))


(defstruct collision
  friction
  elasticity
  surface-velocity)


(ode:define-collision-callback fill-joint-group (in this that)
  (destructuring-bind (universe joint-group) in
    (let* ((geoms (geoms-of universe))
           (this-geom (gethash (cffi:pointer-address this) geoms))
           (that-geom (gethash (cffi:pointer-address that) geoms))
           (contacts-per-collision *contact-points-per-collision*)
           (world (world-of universe))
           (*collision* (make-collision))
           (pre-solve-result (if-let ((pre-solve (pre-solve-of universe)))
                               (funcall pre-solve this-geom that-geom)
                               t)))
      (when pre-solve-result
        ;; todo: move allocation into universe/world/space object
        (c-with ((contact-geoms %ode:contact-geom :count contacts-per-collision))
          (let ((contact-count (%ode:collide this that
                                             contacts-per-collision
                                             contact-geoms
                                             (cffi:foreign-type-size
                                              '%ode:contact-geom))))
            (when (> contact-count 0)
              ;; todo: move allocation into universe/world/space object
              (c-with ((contact %ode:contact :clear t))
                (let ((contacts (%filter-contacts contact-count contact-geoms)))
                  (when-let ((post-solve (post-solve-of universe)))
                    (funcall post-solve this-geom that-geom))
                  (loop for contact-info in contacts do
                    (let* ((contact (fill-contact contact contact-info))
                           (joint (%ode:joint-create-contact world
                                                             joint-group contact))
                           (this-body (%ode:geom-get-body this))
                           (that-body (%ode:geom-get-body that)))
                      (%ode:joint-attach joint this-body that-body))))))))))))


(defun detect-collisions (universe joint-group)
  (with-slots (space) universe
    (ode:space-collide space (list universe joint-group) (ode:collision-callback fill-joint-group))
    joint-group))


(defmacro with-contact-joint-group ((&optional (var (gensym))) universe &body body)
  (once-only (universe)
    `(let ((,var (%ode:joint-group-create 0)))
       (declare (ignorable ,var))
       (unwind-protect
            (progn
              (detect-collisions ,universe ,var)
              ,@body)
         (%ode:joint-group-destroy ,var)))))


(defun %observe-universe (universe seconds-since-last-observation)
  (with-slots (world) universe
    (with-contact-joint-group () universe
      (%ode:world-quick-step world (ode-real seconds-since-last-observation)))))


(defun gravity (universe)
  (c-with ((vec %ode:vector3))
    (%ode:world-get-gravity (world-of universe) vec)
    (vec3 (vec 0) (vec 1) (vec 2))))


(defun (setf gravity) (vec universe)
  (%ode:world-set-gravity (world-of universe)
                          (ode-real (vref vec 0))
                          (ode-real (vref vec 1))
                          (ode-real (vref vec 2)))
  vec)
