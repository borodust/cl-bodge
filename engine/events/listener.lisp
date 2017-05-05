(in-package :cl-bodge.events)


(defclass event-listener ()
  ((callbacks :initform (list))))


(defun register-event-handler (event-listener event-class-name handler emitter)
  (with-slots (callbacks) event-listener
    (push (list event-class-name emitter handler) callbacks)))


(defun subscribe-listener (event-listener)
  (with-slots (callbacks) event-listener
    (dolist (args callbacks)
      (apply #'subscribe-to args))))


(defun unsubscribe-listener (event-listener)
  (with-slots (callbacks) event-listener
    (dolist (args callbacks)
      (apply #'unsubscribe-from args))))
