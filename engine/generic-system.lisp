(in-package :cl-bodge.engine)


(defclass generic-system (system)
  ((state-lock :initform (make-recursive-lock "generic-system-state-lock")
               :reader lock-of)))


(defgeneric initialize-system (system)
  (:method (system) nil))


(defgeneric discard-system (system)
  (:method (system) nil))


(defmacro with-system-lock-held ((system &optional lock-var) &body body)
  (once-only (system)
    `(let ,(unless (null lock-var)
                   `((,lock-var (ge.ng::lock-of ,system))))
       (with-recursive-lock-held ((ge.ng::lock-of ,system))
         ,@body))))


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
