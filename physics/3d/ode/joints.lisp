(cl:in-package :cl-bodge.physics.ode)


(defhandle joint-handle
    :closeform (%ode:joint-destroy *handle-value*))


(defclass joint (ode-object) ())


(defun make-joint (joint-ctor class-name universe this-body that-body)
  (let ((joint (funcall joint-ctor (world-of universe))))
    (%ode:joint-attach joint (handle-value-of this-body) (if (null that-body)
                                                   (cffi:null-pointer)
                                                   (handle-value-of that-body)))
    (make-instance class-name :handle (make-joint-handle joint))))


(defmacro define-joint-class (class-name joint-ctor-name)
  (let ((class-ctor-name (symbolicate 'make- class-name)))
    `(progn
       (defclass ,class-name (joint) ())
       (declaim (inline ,class-ctor-name))
       (defun ,class-ctor-name (universe this-body &optional that-body)
         (make-joint (lambda (world)
                       (,joint-ctor-name world 0))
                     ',class-name universe this-body that-body)))))


(define-joint-class ball-joint %ode:joint-create-ball)
(define-joint-class hinge-joint %ode:joint-create-hinge)
(define-joint-class slider-joint %ode:joint-create-slider)
(define-joint-class universal-joint %ode:joint-create-universal)
(define-joint-class double-hinge-joint %ode:joint-create-hinge2)
(define-joint-class angular-motor-joint %ode:joint-create-a-motor)
