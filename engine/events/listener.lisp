(in-package :cl-bodge.events)


(defclass event-listening ()
  ((lock :initform (bt:make-recursive-lock "event-listener-lock"))
   (enabled-p :initform nil)
   (callbacks :initform (list))))


(defun register-event-handler (event-listener event-class-name emitter handler)
  (with-slots (callbacks lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (subscribe-to event-class-name emitter handler))
      (pushnew (list event-class-name emitter handler) callbacks :test #'equal))))


(defun remove-event-handler (event-listener event-class-name handler emitter)
  (with-slots (callbacks lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (unsubscribe-from event-class-name emitter handler))
      (deletef callbacks (list event-class-name emitter handler) :test #'equal))))


(defun remove-by-event-emitter (event-listener emitter)
  (with-slots (callbacks lock enabled-p) event-listener
    (flet ((remove-emitter (result handler-info)
             (if (eq emitter (second handler-info))
                 (progn
                   (when enabled-p
                     (apply #'unsubscribe-from handler-info))
                   result)
                 (cons result handler-info))))
      (bt:with-recursive-lock-held (lock)
        (setf callbacks (reduce #'remove-emitter callbacks))))))


(defun subscribe-listener (event-listener)
  (with-slots (callbacks lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (error "Listener already subscribed"))
      (dolist (args callbacks)
        (apply #'subscribe-to args))
      (setf enabled-p t))))


(defun unsubscribe-listener (event-listener)
  (with-slots (callbacks lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (unless callbacks
        (error "Listener already unsubscribed"))
      (dolist (args callbacks)
        (apply #'unsubscribe-from args))
      (setf enabled-p nil))))
