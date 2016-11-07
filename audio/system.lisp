(in-package :cl-bodge.audio)


(defstruct (audio-context
             (:conc-name ac-)
             (:constructor make-audio-context (ctx dev)))
  (ctx nil :read-only t)
  (dev nil :read-only t))


(defclass audio-system (thread-bound-system)
  ()
  (:default-initargs :depends-on '(cl-bodge.host:host-system)))


(defmethod make-system-context ((this audio-system))
  (let* ((dev (alc:open-device))
         (ctx (alc:create-context dev)))
    (alc:make-context-current ctx)
    (make-audio-context ctx dev)))


(defmethod destroy-system-context (ctx (this audio-system))
  (alc:make-context-current (cffi:null-pointer))
  (alc:destroy-context (ac-ctx ctx))
  (alc:close-device (ac-dev ctx)))


(declaim (ftype (function () vec3) listener-gain)
         (inline listener-gain))
(defun listener-gain ()
  (sequence->vec3 (al:get-listener :gain)))


(declaim (ftype (function (vec3) *) (setf listener-gain))
         (inline (setf listener-gain)))
(defun (setf listener-gain) (value)
  (al:listener :gain (vec->array value)))


(declaim (ftype (function () vec3) listener-position)
         (inline listener-position))
(defun listener-position ()
  (sequence->vec3 (al:get-listener :position)))


(declaim (ftype (function () vec3) listener-velocity)
         (inline listener-velocity))
(defun listener-velocity ()
  (sequence->vec3 (al:get-listener :velocity)))


(declaim (inline listener-orientation))
(defun listener-orientation ()
  (let ((result (al:get-listener :orientation)))
    (list (sequence->vec3 result)
          (sequence->vec3 (subseq result 3)))))
