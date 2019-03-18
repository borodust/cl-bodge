(cl:in-package :cl-bodge.audio)


(defstruct (audio-context
             (:conc-name ac-)
             (:constructor %make-audio-context (ctx dev)))
  (ctx nil :read-only t)
  (dev nil :read-only t))


(defclass audio-system (generic-system)
  ((context :initform nil)))


(definline audio ()
  (engine-system 'audio-system))


(defun device-enumerable-p (device)
  (%alc:is-extension-present device "ALC_ENUMERATE_ALL_EXT"))


(defun device-name (device)
  (cffi:foreign-string-to-lisp
   (%alc:get-string device
                    (if (device-enumerable-p device)
                        %alc:+all-devices-specifier+
                        %alc:+device-specifier+))))


(defun print-openal-info ()
  (log/debug "~%OpenAL version: ~A~%OpenAL vendor: ~A~%OpenAL renderer: ~A"
             (cffi:foreign-string-to-lisp
              (%al:get-string %al:+version+))
             (cffi:foreign-string-to-lisp
              (%al:get-string %al:+vendor+))
             (cffi:foreign-string-to-lisp
              (%al:get-string %al:+renderer+))))


(defun print-available-devices-info ()
  (log/debug "Available playback devices:~%~A" (device-name (cffi:null-pointer)))
  (log/debug "Available capture devices:~%~A"
             (cffi:foreign-string-to-lisp
              (%alc:get-string (cffi:null-pointer) %alc:+capture-device-specifier+))))


(defun print-device-info (device)
  (claw:c-with ((value %alc:int))
    (log/debug "Selected device: ~A~%ALC version: ~A.~A"
               (device-name device)
               (progn
                 (%alc:get-integerv device %alc:+major-version+ 1 (value &))
                 value)
               (progn
                 (%alc:get-integerv device %alc:+minor-version+ 1 (value &))
                 value))))


(defun make-audio-context ()
  (claw:with-float-traps-masked ()
    (print-available-devices-info)
    (if-let ((dev (%alc:open-device (cffi:null-pointer))))
      (progn
        (print-device-info dev)
        (let ((ctx (%alc:create-context dev (cffi:null-pointer))))
          (%alc:make-context-current ctx)
          (print-openal-info)
          (log/debug "Audio context assigned")
          (%make-audio-context ctx dev)))
      (error "Couldn't open sound device"))))


(defun destroy-audio-context (ctx)
  (%alc:make-context-current (cffi:null-pointer))
  (%alc:destroy-context (ac-ctx ctx))
  (%alc:close-device (ac-dev ctx)))


(defmethod initialize-system :after ((this audio-system))
  (with-slots (context) this
    (setf context (make-audio-context))))


(defmethod discard-system :before ((this audio-system))
  (with-slots (context) this
    (destroy-audio-context context)))


(defun listener-gain ()
  (claw:c-with ((value %al:float))
    (%al:get-listeneri %al:+gain+ (value &))
    value))


(defun (setf listener-gain) (value)
  (%al:listenerf %al:+gain+ (float value 0f0)))


(declaim (ftype (function () (values vec3 &optional)) listener-position)
         (inline listener-position))
(defun listener-position ()
  (static-vectors:with-static-vector (vec 3 :element-type 'single-float)
    (%al:get-listenerfv %al:+position+ (static-vectors:static-vector-pointer vec))
    (sequence->vec3 vec)))


(declaim (ftype (function () (values vec3 &optional)) listener-velocity)
         (inline listener-velocity))
(defun listener-velocity ()
  (static-vectors:with-static-vector (vec 3 :element-type 'single-float)
    (%al:get-listenerfv %al:+velocity+ (static-vectors:static-vector-pointer vec))
    (sequence->vec3 vec)))


(declaim (inline listener-orientation))
(defun listener-orientation ()
  (static-vectors:with-static-vector (vec 6 :element-type 'single-float)
    (%al:get-listenerfv %al:+orientation+ (static-vectors:static-vector-pointer vec))
    (list (sequence->vec3 vec)
          (sequence->vec3 (subseq vec 3)))))
