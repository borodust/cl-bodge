(cl:in-package :cl-bodge.physics.backend)

(defvar *simulation-engines* (make-hash-table))

(defun register-simulation-engine (name-or-names engine)
  (if (listp name-or-names)
      (loop for name in name-or-names
            do (register-simulation-engine name engine))
      (if (gethash name-or-names *simulation-engines*)
          (warn "Redefining simulation engine with name '~A'" name-or-names)
          (setf (gethash name-or-names *simulation-engines*) engine))))


(defun list-simulation-engines ()
  (remove-duplicates
   (loop for engine being the hash-value in *simulation-engines*
         collect engine)))


(defun find-simulation-engine-by-name (name)
  (gethash name *simulation-engines*))

;;;
;;; ENGINE
;;;
(defgeneric simulation-engine-initialize (engine))
(defgeneric simulation-engine-discard (engine))

;;;
;;; UNIVERSE
;;;
(defgeneric simulation-engine-make-universe (engine &key on-pre-solve on-post-solve &allow-other-keys))
(defgeneric simulation-engine-destroy-universe (engine universe))
(defgeneric simulation-engine-observe-universe (engine universe time-step))
(defgeneric simulation-engine-gravity (engine universe))
(defgeneric (setf simulation-engine-gravity) (value engine universe))

;;;
;;; BODY
;;;
(defgeneric simulation-engine-make-mass-for-circle (engine mass radius &key offset))
(defgeneric simulation-engine-make-mass-for-box (engine mass width height &key offset))

(defgeneric simulation-engine-make-mass-for-sphere (engine mass radius &key offset))
(defgeneric simulation-engine-make-mass-for-cuboid (engine mass width height depth &key offset))

(defgeneric simulation-engine-make-rigid-body (engine universe &key mass kinematic))
(defgeneric simulation-engine-destroy-rigid-body (engine rigid-body))

(defgeneric simulation-engine-apply-force (engine body force))
(defgeneric simulation-engine-body-force (engine body))

(defgeneric simulation-engine-apply-torque (engine body torque))
(defgeneric simulation-engine-body-torque (engine body))

(defgeneric simulation-engine-body-mass (engine body))
(defgeneric (setf simulation-engine-body-mass) (value engine body))

(defgeneric simulation-engine-body-position (engine body))
(defgeneric (setf simulation-engine-body-position) (value engine body))

(defgeneric simulation-engine-body-linear-velocity (engine body))
(defgeneric (setf simulation-engine-body-linear-velocity) (value engine body))

(defgeneric simulation-engine-body-rotation (engine body))
(defgeneric (setf simulation-engine-body-rotation) (value engine body))

(defgeneric simulation-engine-body-angular-velocity (engine body))
(defgeneric (setf simulation-engine-body-angular-velocity) (value engine body))


;;;
;;; SHAPE
;;;
(defgeneric simulation-engine-shape-substance (engine shape))
(defgeneric simulation-engine-shape-body (engine shape))
(defgeneric simulation-engine-destroy-shape (engine shape))


(defgeneric simulation-engine-make-sphere-shape (engine universe radius &key body substance))
(defgeneric simulation-engine-make-cuboid-shape (engine universe width height depth
                                                 &key body substance))

(defgeneric simulation-engine-make-segment-shape (engine universe start end &key body substance))
(defgeneric simulation-engine-make-polyline-shape (engine universe points &key body substance))
(defgeneric simulation-engine-make-polygon-shape (engine universe points &key body substance))
(defgeneric simulation-engine-make-box-shape (engine universe width height &key offset body substance))
(defgeneric simulation-engine-make-circle-shape (engine universe radius &key body substance))


;;;
;;; CONTACT
;;;
(defgeneric (setf simulation-engine-collision-friction) (value engine))
(defgeneric (setf simulation-engine-collision-elasticity) (value engine))
(defgeneric (setf simulation-engine-collision-surface-velocity) (value engine))
(defgeneric simulation-engine-collision-surface-velocity (engine))

(defgeneric simulation-engine-contact-normal (engine))
(defgeneric simulation-engine-contact-point (engine))
(defgeneric simulation-engine-contact-depth (engine))


;;;
;;; Joints, Constraints
;;;
(defgeneric simulation-engine-make-damped-spring-constraint (engine universe
                                                             this-body that-body
                                                             rest-length stiffness damping
                                                             &key this-anchor that-anchor))

(defgeneric simulation-engine-make-slide-constraint (engine universe
                                                     this-body that-body
                                                     min max
                                                     &key this-anchor that-anchor))


(defgeneric simulation-engine-make-pin-constraint (engine universe
                                                   this-body that-body
                                                   &key this-anchor that-anchor))

(defgeneric simulation-engine-destroy-constraint (engine joint))
