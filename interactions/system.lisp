(in-package :cl-bodge.interactions)


(defclass interactions-system (enableable generic-system)
  ((input-state :initform (make-input-state))
   (control-ray :initform nil))
  (:default-initargs :depends-on '(host-system
                                   graphics-system
                                   physics-system)))


(definline interactions ()
  (engine-system 'interactions-system))


(defun subscribe-to-events (input)
  (subscribe-body-to (character-input-event (host) (ev character))
    (register-character input character))

    (subscribe-body-to (keyboard-event (host) (ev key state))
      (register-key-action input key state)))


(defmethod initialize-system :after ((this interactions-system))
  (with-slots (input-state) this
    (subscribe-to-events input-state)))
