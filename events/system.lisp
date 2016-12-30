(in-package :cl-bodge.event)


(defvar *predefined-event-callbacks* nil)
(defvar *registration-callback* nil)


(defclass event-system (enableable generic-system)
  ((handler-table :initform (make-hash-table))
   (executor :initform nil)))


(defmethod initialize-system :after ((this event-system))
  (with-slots (executor) this
    (setf executor (acquire-executor))
    (flet ((register-handler (event-class-name handler)
             (subscribe-to event-class-name handler this)))
      (loop for (event-class-name . handler-name) in *predefined-event-callbacks*
           do (register-handler event-class-name handler-name))
      (setf *registration-callback* #'register-handler))))


(definline events ()
  (engine-system 'event-system))


(defmethod discard-system :before ((this event-system))
  (with-slots (executor handler-table) this
    (release-executor executor)
    (clrhash handler-table)
    (setf *registration-callback* nil)))


(defmacro %with-accessor-bindings ((accessor-bindings event-var) &body body)
  (let ((bindings (loop for binding in accessor-bindings
                     for (name accessor) = (if (listp binding)
                                               binding
                                               (list binding binding))
                     collect `(,name (,accessor ,event-var)))))
    `(symbol-macrolet ,bindings
       ,@body)))


(defmacro subscribe-body-to ((event-class
                              (&rest accessor-bindings) &optional (event-var (gensym)))
                                                          event-system &body body)
  `(subscribe-to ',event-class (lambda (,event-var)
                                 (declare (ignorable ,event-var))
                                 (%with-accessor-bindings (,accessor-bindings ,event-var)
                                   ,@body))
                 ,event-system))

;;;
;;;
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun register-predefined-callback (event-class fn-name)
    (unless (assoc event-class *predefined-event-callbacks*)
      (setf (assoc-value *predefined-event-callbacks* event-class) fn-name))
    (when *registration-callback* (funcall *registration-callback* event-class fn-name))))


(defmacro define-event-handler (name event-class (event-var &rest accessor-bindings)
                                 &body body)
  `(progn
     (defun ,name (,event-var)
       (declare (ignorable ,event-var))
       (%with-accessor-bindings (,accessor-bindings ,event-var)
         ,@body))
     (register-predefined-callback ',event-class ',name)))
