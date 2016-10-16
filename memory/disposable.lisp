(in-package :cl-bodge.memory)


(defgeneric destructor-of (obj)
  (:method (obj) '()))


(definline dispose (obj)
  (if (finalizedp obj)
      (error "Attempt to dispose already finalized object.")
      (loop for finalizer in (destructor-of obj) do
           (funcall finalizer)
         finally (setf (slot-value obj 'finalized-p) t))))


(definline %ensure-not-null (value)
  (if (null value)
      (error "Value of slot used in destructor can't be null.
Check define-destructor documentation.")
      value))


(defmacro define-destructor (class-name (&rest slots) &body body)
  (with-gensyms (this finalized-p-holder)
    `(defmethod ge.mem::destructor-of ((,this ,class-name))
       (let ,(loop for slot in slots collecting
                  (if (listp slot)
                      `(,(first slot) (%ensure-not-null (,(second slot) ,this)))
                      `(,slot (%ensure-not-null (slot-value ,this ',slot)))))
         (let ((,finalized-p-holder (slot-value ,this 'finalized-p)))
           (cons (lambda () (unless (holder-value ,finalized-p-holder)
                              ,@body))
                 (call-next-method)))))))


(defstruct holder
  (value nil :type boolean))


(defclass disposable ()
  ((finalized-p :initform (make-holder))))


(defun finalizedp (disposable)
  (holder-value (slot-value disposable 'finalized-p)))


(defmethod initialize-instance :around ((this disposable) &key)
  (call-next-method)
  (if-let ((destructor (destructor-of this)))
    (loop for finalizer in (destructor-of this) do
         (finalize this finalizer))))


(defmacro with-disposable ((var) obj &body body)
  (once-only (obj)
    `(let ((,var ,obj))
       (unwind-protect
            (progn
              ,@body)
         (dispose ,var)))))


(defclass disposable-container ()
  ((disposables :initform (make-weak-hash-table :weakness :key))))
