(cl:in-package :cl-bodge.events)


(defclass event-hub (event-emitting)
  ((listener :initform (make-instance 'event-listening))))


(defun make-event-hub ()
  (make-instance 'event-hub))


(defgeneric register-event-emitter (hub emitter &rest event-class-names)
  (:method ((hub event-hub) emitter &rest event-class-names)
    (with-slots (listener) hub
      (flet ((chain-fire (ev)
               (fire-event ev hub)))
        (dolist (class-name event-class-names)
          (register-event-handler listener class-name emitter #'chain-fire))))))


(defgeneric deregister-event-emitter (hub emitter)
  (:method ((hub event-hub) emitter)
    (with-slots (listener) hub
      (deregister-by-event-emitter listener emitter))))


(defun enable-hub (hub)
  (with-slots (listener) hub
    (subscribe-listener listener)))


(defun disable-hub (hub)
  (with-slots (listener) hub
    (unsubscribe-listener listener)))
