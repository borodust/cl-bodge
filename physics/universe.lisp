(in-package :cl-bodge.physics)


(defvar *contact-points-per-collision* 1)


(defclass universe ()
  ((world :initform (%ode:world-create) :reader world-of)
   (space :initform (%ode:hash-space-create (cffi:null-pointer)) :reader space-of)
   (geoms :initform (tg:make-weak-hash-table :weakness :value :test 'eql) :reader geoms-of)
   (collision-callbacks :initform '() :reader collision-callbacks-of)))


(defun %register-geom (universe geom)
  (with-slots (geoms) universe
    (setf (gethash (id-of geom) geoms) geom)))


(defun %register-collision-callback (universe callback)
  (with-slots (collision-callbacks) universe
    (pushnew callback collision-callbacks)))


(defmethod initialize-instance :after ((this universe) &key)
  (with-slots (world) this
    (%ode:world-set-erp world 0.8)
    (%ode:world-set-cfm world 0.01)))


(defun make-universe ()
  (make-instance 'universe))


(defun destroy-universe (uni)
  (with-slots (world space collision-callbacks) uni
    (setf collision-callbacks '())
    (%ode:world-destroy world)
    (%ode:space-destroy space)))


(defun initialize-contact (contact contact-geom)
  (setf (%ode:contact.surface.mode contact) (mask 'contact-flags :approx0 :bounce))
  (setf (%ode:contact.surface.mu contact) +infinity+)
  (setf (%ode:contact.surface.bounce contact) 1.0)
  (copy-memory-autowrapped (%ode:contact.geom& contact)
                           (ptr contact-geom)
                           '%ode:contact-geom)
  contact)


(define-collision-callback fill-joint-group (in this that)
  (unless (loop with geoms = (geoms-of (universe))
             for cb in (collision-callbacks-of (universe))
             for processed-p = (funcall cb (gethash this geoms) (gethash that geoms))
             until processed-p
             finally (return processed-p))
    (destructuring-bind (world joint-group) in
      (c-with ((contact-geoms %ode:contact-geom :count *contact-points-per-collision*))
        (let ((contact-count (%ode:collide this that
                                           *contact-points-per-collision*
                                           contact-geoms
                                           (foreign-type-size
                                            (find-type '%ode:contact-geom)))))
          (when (> contact-count 0)
            (c-with ((contacts %ode:contact :count contact-count :calloc t))
              (loop for i from 0 below contact-count do
                   (let* ((contact (initialize-contact
                                    (c-ref contacts %ode:contact i)
                                    (c-ref contact-geoms %ode:contact-geom i)))
                          (joint (%ode:joint-create-contact world joint-group contact))
                          (this-body (%ode:geom-get-body this))
                          (that-body (%ode:geom-get-body that)))
                     (%ode:joint-attach joint this-body that-body))))))))))


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
      (%ode:world-quick-step world #f seconds-since-last-observation))))
