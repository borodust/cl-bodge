(cl:in-package :cl-bodge.engine)

;;;
;;; Events
;;;
(defun post (event-or-class &rest initargs &key &allow-other-keys)
  (let ((event (etypecase event-or-class
                 ((or symbol standard-class) (apply #'make-instance event-or-class initargs))
                 (event event-or-class))))
    (run (concurrently ()
           (fire-event event (event-emitter-of (engine)))))))


(defun subscribe (event-class handler)
  (subscribe-to event-class (event-emitter-of (engine)) handler))


(defun unsubscribe (event-class handler)
  (unsubscribe-from event-class (event-emitter-of (engine)) handler))


(defmacro define-event-handler (name ((event event-class-name) &rest accessor-bindings)
                                &body body)
  `(progn
     (defun ,name (,event)
       (declare (ignorable ,event))
       (ge.eve::%with-accessor-bindings (,accessor-bindings
                                         ,(package-name (symbol-package event-class-name))
                                         ,event)
         ,@body))
     (register-predefined-callback ',event-class-name ',name)))


;;
(defclass subscribing ()
  ((listener :initform (make-instance 'event-listening))))


(defun add-event-handler (subscriber event-class handler)
  (with-slots (listener) subscriber
    (register-event-handler listener event-class (event-emitter-of (engine)) handler)))


(defun remove-event-handler (subscriber event-class handler)
  (with-slots (listener) subscriber
    (deregister-event-handler listener event-class (event-emitter-of (engine)) handler)))


(defun employ-subscriber (subscriber)
  (with-slots (listener) subscriber
    (subscribe-listener listener)))


(defun dismiss-subscriber (subscriber)
  (with-slots (listener) subscriber
    (unsubscribe-listener listener)))


(defmacro subscribe-body ((event-class (&rest accessor-bindings)
                                       &optional (event-var (gensym)))
                          &body body)
  `(subscribe-body-to (,event-class (event-emitter-of (engine)) ,accessor-bindings ,event-var)
     ,@body))
