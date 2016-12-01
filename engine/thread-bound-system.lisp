(in-package :cl-bodge.engine)


(declaim (special *system-context*
                  *system*))


(defclass thread-bound-system (enableable generic-system)
  ((executor :initform nil :accessor %executor-of)
   (context :initform nil)))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system)
             nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defmethod dispatch ((this thread-bound-system) fn &optional (priority :medium))
  (execute (%executor-of this) fn priority))


(defmethod enable ((this thread-bound-system))
  (call-next-method)
  (setf (%executor-of this)
        (acquire-executor :single-threaded-p t :exclusive-p t
                          :special-variables '(*system-context*
                                               *system*)))
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (log-errors
                 (setf *system-context* (make-system-context this)
                       *system* this)
                 (open-latch latch))))))


(defmethod disable ((this thread-bound-system))
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (destroy-system-context *system-context* this)
               (open-latch latch))))
  (release-executor (%executor-of this))
  (call-next-method))


(declaim (inline check-system-context))
(defun check-system-context ()
  (unless (boundp '*system-context*)
    (error "*system-context* is unbound")))


;;
(defclass thread-bound-object (system-object) ())


(defmacro define-system-function (name system-class lambda-list &body body)
  (multiple-value-bind (forms decls doc) (parse-body body :documentation t)
    `(defun ,name ,lambda-list
       ,@(when doc (list doc))
       ,@decls
       ;; todo : disable in production
       (unless (subtypep (class-of *system*) ',system-class)
         (error "~a executed in the wrong system thread: required ~a, but got ~a"
                ',name ',system-class (class-name (class-of *system*))))
       ,@forms)))
