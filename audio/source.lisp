(cl:in-package :cl-bodge.audio)


(defhandle audio-source-handle
  :initform (claw:c-with ((source-id %al:uint))
              (%al:gen-sources 1 (source-id &))
              source-id)
  :closeform (claw:c-with ((source-id %al:uint :value *handle-value*))
               (%al:delete-sources 1 (source-id &))))


(defclass audio-source (al-object) ()
  (:default-initargs :handle (make-audio-source-handle)))


(defun make-audio-source ()
  (make-instance 'audio-source))


(declaim (ftype (function (audio-buffer audio-source) *) attach-buffer)
         (inline attach-buffer))
(defun attach-audio-buffer (buffer source)
  (%al:sourcei (handle-value-of source) %al:+buffer+ (handle-value-of buffer)))


(declaim (ftype (function (audio-source) *) play-audio)
         (inline play-audio))
(defun play-audio (source)
  (%al:source-play (handle-value-of source)))


(declaim (ftype (function (audio-source) *) stop-audio)
         (inline stop-audio))
(defun stop-audio (source)
  (%al:source-stop (handle-value-of source)))


(declaim (ftype (function (audio-source) *) pause-audio)
         (inline pause-audio))
(defun pause-audio (source)
  (%al:source-pause (handle-value-of source)))


(defun audio-looped-p (source)
  (claw:c-with ((value %al:int))
    (%al:get-sourcei (handle-value-of source) %al:+looping+ (value &))
    (= %al:+true+ value)))


(defun (setf audio-looped-p) (value source)
  (%al:sourcei (handle-value-of source) %al:+looping+ (if value %al:+true+ %al:+false+)))


(defun audio-gain (source)
  (claw:c-with ((value %al:float))
    (%al:get-sourcef (handle-value-of source) %al:+gain+ (value &))
    value))


(defun (setf audio-gain) (value source)
  (%al:sourcef (handle-value-of source) %al:+gain+ (f value)))


(defun audio-pitch (source)
  (claw:c-with ((value %al:float))
    (%al:get-sourcef (handle-value-of source) %al:+pitch+ (value &))
    value))


(defun (setf audio-pitch) (value source)
  (%al:sourcef (handle-value-of source) %al:+pitch+ (f value)))
