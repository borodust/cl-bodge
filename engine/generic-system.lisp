(in-package :cl-bodge.engine)


(defclass generic-system (lockable system)
  ()
  (:documentation "Base class for systems with generic behaviour: simple lifecycle, lockable"))


(defgeneric initialize-system (system)
  (:documentation "Initialize system during engine startup. Prefer :after methods.")
  (:method (system) nil))


(defgeneric discard-system (system)
  (:documentation "Discard system during engine shutdown. Prefer :before methods.")
  (:method (system) nil))


(defmacro with-system-lock-held ((system &optional lock-var) &body body)
  "Hold system's lock during `body`."
  `(with-instance-lock-held (,system ,lock-var)
     ,@body))


(defmethod enabledp :around ((this generic-system))
  (with-system-lock-held (this)
    (call-next-method)))


(defun system-class-name-of (this)
  (class-name (class-of this)))


(defmethod enable ((this generic-system))
  (call-next-method)
  (initialize-system this))


(defmethod enable :around ((this generic-system))
  (with-system-lock-held (this)
    (when (enabledp this)
      (error "~a already enabled" (system-class-name-of this)))
    (call-next-method)))


(defmethod disable ((this generic-system))
  (discard-system this))


(defmethod disable :around ((this generic-system))
  (with-system-lock-held (this)
    (unless (enabledp this)
      (error "~a already disabled" (system-class-name-of this)))
    (call-next-method)))
