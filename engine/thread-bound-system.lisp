(in-package :cl-bodge.engine)


(declaim (special *system-context*
                  *system*))


(defclass thread-bound-system (enableable generic-system)
  ((executor :initform nil :accessor %executor-of)
   (context :initform nil :reader system-context-of)))


(defgeneric make-system-context (system)
  (:method (system)
    (declare (ignore system)
             nil)))


(defgeneric destroy-system-context (context system)
  (:method (context system) (declare (ignore context system))))


(defmethod dispatch ((this thread-bound-system) fn &key (priority :medium) important-p)
  (unless (call-next-method)
    (flet ((invoker ()
             (log-errors
               (let ((*system-context* (system-context-of this))
                     (*system* this))
                 (funcall fn)))))
      (execute (%executor-of this) #'invoker :priority priority :important-p important-p))
    t))


(defgeneric acquire-system-executor (system)
  (:method ((this thread-bound-system))
    (acquire-executor :single-threaded-p t :exclusive-p t)))


(defgeneric release-system-executor (system executor)
  (:method ((this thread-bound-system) executor)
    (release-executor executor)))


(defmethod enable ((this thread-bound-system))
  (call-next-method)
  (setf (%executor-of this) (acquire-system-executor this))
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (log-errors
                 (with-slots (context) this
                   (setf context (make-system-context this)))
                 (open-latch latch)))
             :priority :highest)))


(defmethod disable ((this thread-bound-system))
  (wait-with-latch (latch)
    (execute (%executor-of this)
             (lambda ()
               (unwind-protect
                    (destroy-system-context this (system-context-of this))
                 (open-latch latch)))
             :priority :highest))
  (release-system-executor this (%executor-of this))
  (call-next-method))


(defmacro define-system-function (name system-class lambda-list &body body)
  (multiple-value-bind (forms decls doc) (parse-body body :documentation t)
    `(defun ,name ,lambda-list
       ,@(when doc (list doc))
       ,@decls
       (when-debugging
         (cond
           ((or (not (boundp '*system*)) (null *system*))
            (error (concatenate 'string "~a executed in the wrong system thread:"
                                " *system* unbound or nil, but ~a required")
                   ',name ',system-class))
           ((not (subtypep (class-of *system*) ',system-class))
            (error "~a executed in the wrong system thread: required ~a, but got ~a"
                   ',name ',system-class (and (class-name (class-of *system*)))))))
       ,@forms)))
