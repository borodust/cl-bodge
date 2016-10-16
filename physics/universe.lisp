(in-package :cl-bodge.physics)


(defvar *contact-points-per-collision* 1)


(defclass universe ()
  ((world :initform (ode:world-create) :reader world-of)
   (space :initform (ode:hash-space-create (cffi:null-pointer)) :reader space-of)))


(defun make-universe ()
  (make-instance 'universe))


(defun destroy-universe (uni)
  (with-slots (world space) uni
    (ode:world-destroy world)
    (ode:space-destroy space)))


(cffi:defcstruct collision-data
  (joint-group ode:joint-group-id)
  (world ode:world-id))


(defun initialize-contact (contact-ptr contact-point-ptr)
  (cffi:with-foreign-slots (((:pointer ode:surface) (:pointer ode:geom))
                            contact-ptr (:struct ode:contact))
    (cffi:with-foreign-slots ((ode:mode) ode:surface (:struct ode:surface-parameters))
      (setf ode:mode 0))
    (copy-memory ode:geom contact-point-ptr '(:struct ode:contact-geom)))
  contact-ptr)



(cffi:defcallback collision-callback :void ((data-ptr :pointer)
                                            (g1 ode:geom-id) (g2 ode:geom-id))
  (cffi:with-foreign-object (contact-points-ptr '(:struct ode:contact-geom)
                                                *contact-points-per-collision*)
      (let ((contact-count (ode:collide g1 g2 *contact-points-per-collision* contact-points-ptr
                                        (cffi:foreign-type-size '(:struct ode:contact-geom)))))
        (when (> contact-count 0)
          (cffi:with-foreign-slots ((joint-group world) data-ptr (:struct collision-data))
            (cffi:with-foreign-object (contacts-ptr '(:struct ode:contact) contact-count)
              (loop for i from 0 below contact-count do
                   (let* ((contact-ptr (initialize-contact
                                        (cffi:mem-aptr contacts-ptr '(:struct ode:contact) i)
                                        (cffi:mem-aptr contact-points-ptr
                                                       '(:struct ode:contact-geom) i)))
                          (joint (ode:joint-create-contact world joint-group contact-ptr))
                          (b1 (ode:geom-get-body g1))
                          (b2 (ode:geom-get-body g2)))
                     (ode:joint-attach joint b1 b2)))))))))


(defun detect-collisions (universe)
  (with-slots (space (uni-world world)) universe
    (cffi:with-foreign-object (data-ptr '(:struct collision-data))
      (cffi:with-foreign-slots ((joint-group world) data-ptr (:struct collision-data))
        (setf joint-group (ode:joint-group-create 0)
              world uni-world)
        (ode:space-collide space data-ptr (cffi:callback collision-callback))
        joint-group))))


(defmacro with-contact-joint-group ((&optional (var (gensym))) universe &body body)
  (once-only (universe)
    `(let ((,var (detect-collisions ,universe)))
       (declare (ignorable ,var))
       (unwind-protect
            (progn
              ,@body)
         (ode:joint-group-destroy ,var)))))


(defun %observe-universe (universe seconds-since-last-observation)
  (with-slots (world) universe
    (with-contact-joint-group () universe
      (ode:world-step world #f seconds-since-last-observation))))
