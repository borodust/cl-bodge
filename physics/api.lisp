(cl:in-package :cl-bodge.physics)


(defclass simulation-handle (disposable)
  ((engine :initarg :engine :initform (error ":engine missing") :reader %engine-of)
   (handle :initarg :handle :initform (error ":handle missing") :reader %handle-of)))


(defmethod %handle-of ((this null))
  (declare (ignore this))
  nil)

;;;
;;; UNIVERSE
;;;
(defclass universe-handle (simulation-handle) ())


(defun make-universe-handle (engine handle)
  (make-instance 'universe-handle :engine engine :handle handle))


(define-destructor universe-handle ((engine %engine-of) (handle %handle-of))
  (simulation-engine-destroy-universe engine handle))


(defstruct substance-wrapper
  (shape nil))


(defun make-universe (simulation-engine-name &rest args
                      &key on-pre-solve on-post-solve &allow-other-keys)
  (if-let ((engine (find-simulation-engine-by-name simulation-engine-name)))
    (labels ((%shape-handle-of (engine-shape)
               (substance-wrapper-shape (simulation-engine-shape-substance engine engine-shape)))
             (%pre-solve (this that)
               (if on-pre-solve
                   (funcall on-pre-solve (%shape-handle-of this) (%shape-handle-of that))
                   t))
             (%post-solve (this that)
               (when on-post-solve
                   (funcall on-post-solve (%shape-handle-of this) (%shape-handle-of that)))))
      (make-universe-handle engine (apply #'simulation-engine-make-universe engine
                                          :on-pre-solve #'%pre-solve
                                          :on-post-solve #'%post-solve args)))
    (error "Simulation engine with name '~A' is not registered" simulation-engine-name)))


(defun observe-universe (universe time-step)
  (simulation-engine-observe-universe (%engine-of universe) (%handle-of universe) time-step))


(defun (setf gravity) (value universe)
  (setf (simulation-engine-gravity (%engine-of universe) (%handle-of universe)) value))


(defun gravity (universe)
  (simulation-engine-gravity (%engine-of universe) (%handle-of universe)))


;;;
;;; RIGID BODY
;;;
(defclass rigid-body-handle (simulation-handle) ())


(defun make-rigid-body-handle (engine handle)
  (make-instance 'rigid-body-handle :engine engine :handle handle))


(define-destructor rigid-body-handle ((engine %engine-of) (handle %handle-of))
  (simulation-engine-destroy-rigid-body engine handle))


(defun make-rigid-body (universe &key mass)
  (make-rigid-body-handle (%engine-of universe)
                          (simulation-engine-make-rigid-body (%engine-of universe)
                                                             (%handle-of universe)
                                                             :mass mass)))


(defun infuse-circle-mass (body mass radius)
  (setf body (simulation-engine-make-mass-for-circle (%engine-of body)
                                                     mass radius)))


(defun apply-force (rigid-body force-vec)
  (simulation-engine-apply-force (%engine-of rigid-body)
                                 (%handle-of rigid-body)
                                 force-vec))


(defun body-force (rigid-body)
  (simulation-engine-body-force (%engine-of rigid-body)
                                (%handle-of rigid-body)))


(defun apply-torque (rigid-body torque)
  (simulation-engine-apply-torque (%engine-of rigid-body)
                                  (%handle-of rigid-body)
                                  torque))


(defun body-torque (rigid-body)
  (simulation-engine-body-torque (%engine-of rigid-body)
                                 (%handle-of rigid-body)))


(defun body-position (body)
  (simulation-engine-body-position (%engine-of body)
                                   (%handle-of body)))


(defun (setf body-position) (value body)
  (setf (simulation-engine-body-position (%engine-of body)
                                         (%handle-of body))
        value))


(defun body-rotation (body)
  (simulation-engine-body-rotation (%engine-of body)
                                   (%handle-of body)))


(defun (setf body-rotation) (value body)
  (setf (simulation-engine-body-rotation (%engine-of body)
                                         (%handle-of body))
        value))


(defun body-linear-velocity (body)
  (simulation-engine-body-linear-velocity (%engine-of body)
                                          (%handle-of body)))


(defun (setf body-linear-velocity) (value body)
  (setf (simulation-engine-body-linear-velocity (%engine-of body)
                                                (%handle-of body))
        value))


(defun body-angular-velocity (body)
  (simulation-engine-body-angular-velocity (%engine-of body)
                                           (%handle-of body)))


(defun (setf body-angular-velocity) (value body)
  (setf (simulation-engine-body-angular-velocity (%engine-of body)
                                                 (%handle-of body))
        value))

;;;
;;; SHAPES
;;;
(defclass shape-handle (simulation-handle)
  ((substance :initform nil :initarg :substance)
   (body :initform nil :initarg :body)))


(defun make-shape-handle (engine body substance handle-ctor)
  (let* ((substance-wrapper (make-substance-wrapper))
         (shape (make-instance 'shape-handle :engine engine
                                             :body body
                                             :handle (funcall handle-ctor substance-wrapper)
                                             :substance substance)))
    (setf (substance-wrapper-shape substance-wrapper) shape)))


(define-destructor shape-handle ((engine %engine-of) (handle %handle-of))
  (simulation-engine-destroy-shape engine handle))


(defun shape-substance (shape)
  (with-slots (substance) shape
    substance))


(defun shape-body (shape)
  (with-slots (body) shape
    body))

;;;
;;; 3D SHAPES
;;;
(defun make-plane-shape (universe)
  (declare (ignore universe)))
(defun make-sphere-shape (universe)
  (declare (ignore universe)))
(defun make-cube-shape (universe)
  (declare (ignore universe)))


;;;
;;; 2D SHAPES
;;;
(defun make-segment-shape (universe start end &key body substance)
  (make-shape-handle (%engine-of universe)
                     body
                     substance
                     (lambda (prepared-substance)
                       (simulation-engine-make-segment-shape (%engine-of universe)
                                                             (%handle-of universe)
                                                             start end
                                                             :body (%handle-of body)
                                                             :substance prepared-substance))))


(defun make-polyline-shape (universe points &key body substance)
  (make-shape-handle (%engine-of universe)
                     body
                     substance
                     (lambda (prepared-substance)
                       (simulation-engine-make-polyline-shape (%engine-of universe)
                                                            (%handle-of universe)
                                                            points
                                                            :body (%handle-of body)
                                                            :substance prepared-substance))))


(defun make-polygon-shape (universe points &key body substance)
  (make-shape-handle (%engine-of universe)
                     body
                     substance
                     (lambda (prepared-substance)
                       (simulation-engine-make-polygon-shape (%engine-of universe)
                                                             (%handle-of universe)
                                                             points
                                                             :body (%handle-of body)
                                                             :substance prepared-substance))))


(defun make-box-shape (universe width height &key offset body substance)
  (make-shape-handle (%engine-of universe)
                     body
                     substance
                     (lambda (prepared-substance)
                       (simulation-engine-make-box-shape (%engine-of universe)
                                                         (%handle-of universe)
                                                         width height
                                                         :body (%handle-of body)
                                                         :offset offset
                                                         :substance prepared-substance))))


(defun make-circle-shape (universe radius &key body substance)
  (make-shape-handle (%engine-of universe)
                     body
                     substance
                     (lambda (prepared-substance)
                       (simulation-engine-make-circle-shape (%engine-of universe)
                                                            (%handle-of universe)
                                                            radius
                                                            :body (%handle-of body)
                                                            :substance prepared-substance))))


;;;
;;; Contacts
;;;
(defun has-next-shape-contact ())


(defun next-shape-contact ())


(defun contact-friction ())


(defun contact-elasticity ())


(defun contact-surface-velocity ())


(defun contact-normal ())


(defun contact-point ())


(defun contact-depth ())


(defun contact-this-shape ())


(defun contact-that-shape ())
