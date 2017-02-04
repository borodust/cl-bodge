(in-package :cl-bodge.physics)


(defvar *contact-points-per-collision* 4)


(defclass universe ()
  ((world :initform (%ode:world-create) :reader world-of)
   (space :initform (%ode:hash-space-create (cffi:null-pointer)) :reader space-of)
   (geoms :initform (tg:make-weak-hash-table :weakness :value :test 'eql) :reader geoms-of)))


(defun %register-geom (universe geom)
  (with-slots (geoms) universe
    (let ((ptr-addr (cffi:pointer-address (ptr (handle-value-of geom)))))
      (setf (gethash ptr-addr geoms) geom))))


(defmethod initialize-instance :after ((this universe) &key)
  (with-slots (world) this
    (%ode:world-set-erp world (ode-real 0.8))
    (%ode:world-set-cfm world (ode-real 0.01))))


(defun make-universe ()
  (make-instance 'universe))


(defun destroy-universe (uni)
  (with-slots (world space collision-callbacks) uni
    (setf collision-callbacks '())
    (%ode:world-destroy world)
    (%ode:space-destroy space)))


(defun %filter-contacts (contact-count contact-geoms this-geom that-geom)
  (let* ((contacts (loop for i from 0 below contact-count
                      collecting (make-contact (c-ref contact-geoms %ode:contact-geom i)))))
    (filter-contacts contacts this-geom that-geom)))


(define-collision-callback fill-joint-group (in this that)
  (destructuring-bind (universe joint-group) in
    (let* ((geoms (geoms-of universe))
           (this-geom (gethash (cffi:pointer-address (ptr this)) geoms))
           (that-geom (gethash (cffi:pointer-address (ptr that)) geoms))
           (world (world-of universe)))
      (unless (collide this-geom that-geom)
        ;; todo: move allocation into universe/world/space object
        (c-with ((contact-geoms %ode:contact-geom :count *contact-points-per-collision*))
          (let ((contact-count (%ode:collide this that
                                             *contact-points-per-collision*
                                             contact-geoms
                                             (foreign-type-size
                                              (find-type '%ode:contact-geom)))))
            (when (> contact-count 0)
              ;; todo: move allocation into universe/world/space object
              (c-with ((contact %ode:contact :calloc t))
                (let ((contacts (%filter-contacts contact-count contact-geoms
                                                  this-geom that-geom)))
                  (loop for contact-info in contacts do
                       (let* ((contact (fill-contact contact contact-info))
                              (joint (%ode:joint-create-contact world
                                                                joint-group contact))
                              (this-body (%ode:geom-get-body this))
                              (that-body (%ode:geom-get-body that)))
                         (%ode:joint-attach joint this-body that-body))))))))))))


(defun detect-collisions (universe joint-group)
  (with-slots (space) universe
    (space-collide space (list universe joint-group) (collision-callback fill-joint-group))
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
