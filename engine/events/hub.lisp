(in-package :cl-bodge.events)


(defclass event-hub (event-emitter event-listener)
  ())


(defun make-event-hub ()
  (make-instance 'event-hub))


(defun register-emitter (hub emitter &rest event-class-names)
  (flet ((chain-fire (ev)
           (fire-event ev hub)))
    (dolist (class-name event-class-names)
      (register-event-handler hub class-name #'chain-fire emitter))))


(defun remove-emitter (hub emitter)
  (remove-by-emitter hub emitter))
