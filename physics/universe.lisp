(in-package :cl-bodge.physics)


(defvar *contact-points-per-collision* 1)


(defclass universe ()
  ((world :initform (%ode:world-create) :reader world-of)
   (space :initform (%ode:hash-space-create (cffi:null-pointer)) :reader space-of)))


(defun make-universe ()
  (make-instance 'universe))


(defun destroy-universe (uni)
  (with-slots (world space) uni
    (%ode:world-destroy world)
    (%ode:space-destroy space)))


(defun initialize-contact (contact contact-point)
  (macrolet ((contact-ref (&rest fields)
               `(c-ref contact %ode:contact ,@fields)))
    (setf (contact-ref :surface :mode) 0)
    (copy-memory-autowrapped (contact-ref :geom &)
                             (ptr contact-point)
                             '%ode:contact-geom))
    contact)


(define-collision-callback fill-joint-group (in this that)
  (destructuring-bind (world joint-group) in
    (with-alloc (contact-points '%ode:contact-geom *contact-points-per-collision*)
      (let ((contact-count (%ode:collide this that
                                         *contact-points-per-collision*
                                         contact-points
                                         (foreign-type-size
                                          (find-type '%ode:contact-geom)))))
        (when (> contact-count 0)
          (with-alloc (contacts '%ode:contact contact-count)
            (loop for i from 0 below contact-count do
                 (let* ((contact (initialize-contact
                                  (c-aref contacts i '%ode:contact)
                                  (c-aref contact-points i '%ode:contact-geom)))
                        (joint (%ode:joint-create-contact world joint-group contact))
                        (this-body (%ode:geom-get-body this))
                        (that-body (%ode:geom-get-body that)))
                   (%ode:joint-attach joint this-body that-body)))))))))


(defun detect-collisions (universe)
  (with-slots (space world) universe
    (let ((joint-group (%ode:joint-group-create 0)))
      (space-collide space (list world joint-group) (collision-callback fill-joint-group))
      joint-group)))


(defmacro with-contact-joint-group ((&optional (var (gensym))) universe &body body)
  (once-only (universe)
    `(let ((,var (detect-collisions ,universe)))
       (declare (ignorable ,var))
       (unwind-protect
            (progn
              ,@body)
         (%ode:joint-group-destroy ,var)))))


(defun %observe-universe (universe seconds-since-last-observation)
  (with-slots (world) universe
    (with-contact-joint-group () universe
      (%ode:world-step world #f seconds-since-last-observation))))
