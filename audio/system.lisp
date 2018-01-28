(in-package :cl-bodge.audio)


(defstruct (audio-context
             (:conc-name ac-)
             (:constructor make-audio-context (ctx dev)))
  (ctx nil :read-only t)
  (dev nil :read-only t))


(defclass audio-system (thread-bound-system)
  ()
  (:default-initargs :depends-on '(cl-bodge.host:host-system)))


(definline audio ()
  (engine-system 'audio-system))

(defun device-enumerable-p (device)
  (alc:extension-present-p device "ALC_ENUMERATE_ALL_EXT"))


(defun device-name (device)
  (alc:get-string device
                  (if (device-enumerable-p device)
                      :all-devices-specifier
                      :device-specifier)))


(defun print-openal-info ()
  (log:debug "~%OpenAL version: ~A~%OpenAL vendor: ~A~%OpenAL renderer: ~A"
             (al:get-string :version)
             (al:get-string :vendor)
             (al:get-string :renderer)))


(defun print-available-devices-info ()
  (log:debug "Available playback devices:~%~A" (device-name (cffi:null-pointer)))
  (log:debug "Available capture devices:~%~A"
             (alc:get-string (cffi:null-pointer) :capture-device-specifier)))


(defun print-device-info (device)
  (log:debug "Selected device: ~A~%ALC version: ~A.~A"
             (device-name device)
             (first (alc:get-integer device :major-version))
             (first (alc:get-integer device :minor-version))))


(defmethod make-system-context ((this audio-system))
  (claw:with-float-traps-masked ()
    (print-available-devices-info)
    (if-let ((dev (alc:open-device)))
      (progn
        (print-device-info dev)
        (let ((ctx (alc:create-context dev)))
          (alc:make-context-current ctx)
          (print-openal-info)
          (log:debug "Audio context assigned")
          (make-audio-context ctx dev)))
      (error "Couldn't open sound device"))))


(defmethod destroy-system-context ((this audio-system) ctx)
  (alc:make-context-current (cffi:null-pointer))
  (alc:destroy-context (ac-ctx ctx))
  (alc:close-device (ac-dev ctx)))


(declaim (ftype (function () (values vec3 &optional)) listener-gain)
         (inline listener-gain))
(defun listener-gain ()
  (sequence->vec3 (al:get-listener :gain)))


(declaim (ftype (function (vec3) *) (setf listener-gain))
         (inline (setf listener-gain)))
(defun (setf listener-gain) (value)
  (al:listener :gain (vec->array value)))


(declaim (ftype (function () (values vec3 &optional)) listener-position)
         (inline listener-position))
(defun listener-position ()
  (sequence->vec3 (al:get-listener :position)))


(declaim (ftype (function () (values vec3 &optional)) listener-velocity)
         (inline listener-velocity))
(defun listener-velocity ()
  (sequence->vec3 (al:get-listener :velocity)))


(declaim (inline listener-orientation))
(defun listener-orientation ()
  (let ((result (al:get-listener :orientation)))
    (list (sequence->vec3 result)
          (sequence->vec3 (subseq result 3)))))
