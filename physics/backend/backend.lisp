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

(defgeneric simulation-engine-make-rigid-body (engine universe &key mass))
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


(defgeneric simulation-engine-make-segment-shape (engine universe start end &key body substance))
(defgeneric simulation-engine-make-polyline-shape (engine universe points &key body substance))
(defgeneric simulation-engine-make-polygon-shape (engine universe points &key body substance))
(defgeneric simulation-engine-make-box-shape (engine universe width height &key offset body substance))
(defgeneric simulation-engine-make-circle-shape (engine universe radius &key body substance))


;;;
;;; CONTACT
;;;
(defgeneric (setf simulation-engine-contact-friction) (value engine contact))
(defgeneric (setf simulation-engine-contact-elasticity) (value engine contact))
(defgeneric (setf simulation-engine-contact-surface-velocity) (value engine contact))

(defgeneric simulation-engine-contact-normal (engine contact))
(defgeneric simulation-engine-contact-point (engine contact))
(defgeneric simulation-engine-contact-depth (engine contact))
(defgeneric simulation-engine-contact-this-shape (engine contact))
(defgeneric simulation-engine-contact-that-shape (engine contact))
