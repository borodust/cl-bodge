(in-package :cl-bodge.events)


(defclass event-listener ()
  ((callbacks :initform (list))))


(defun register-event-handler (event-listener event-class-name handler emitter)
  (with-slots (callbacks) event-listener
    (pushnew (list event-class-name emitter handler) callbacks :test #'equal)))


(defun remove-event-handler (event-listener event-class-name handler emitter)
  (with-slots (callbacks) event-listener
    (deletef callbacks (list event-class-name emitter handler) :test #'equal)))


(defun remove-by-emitter (event-listener emitter)
  (with-slots (callbacks) event-listener
    (flet ((remove-emitter (result handler-info)
             (if (eq emitter (second handler-info))
                 result
                 (progn
                   (apply #'unsubscribe-from handler-info)
                   (cons result handler-info)))))
      (setf callbacks (reduce #'remove-emitter callbacks)))))


(defun subscribe-listener (event-listener)
  (with-slots (callbacks) event-listener
    (when callbacks
      (error "Listener already subscribed"))
    (dolist (args callbacks)
      (apply #'subscribe-to args))))


(defun unsubscribe-listener (event-listener)
  (with-slots (callbacks) event-listener
    (unless callbacks
      (error "Listener already unsubscribed"))
    (dolist (args callbacks)
      (apply #'unsubscribe-from args))))
