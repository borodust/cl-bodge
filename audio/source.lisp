(in-package :cl-bodge.audio)


(defclass audio-source (al-object) ()
  (:default-initargs :id (al:gen-source)))


(define-system-function make-audio-source audio-system (&key (system *system*))
  (make-instance 'audio-source :system system))


(declaim (ftype (function (audio-buffer audio-source) *) attach-buffer)
         (inline attach-buffer))
(defun attach-audio-buffer (buffer source)
  (al:source (handle-value-of source) :buffer (handle-value-of buffer)))


(declaim (ftype (function (audio-source) *) play-audio)
         (inline play-audio))
(defun play-audio (source)
  (al:source-play (handle-value-of source)))


(declaim (ftype (function (audio-source) *) stop-audio)
         (inline stop-audio))
(defun stop-audio (source)
  (al:source-stop (handle-value-of source)))


(declaim (ftype (function (audio-source) *) pause-audio)
         (inline pause-audio))
(defun pause-audio (source)
  (al:source-pause (handle-value-of source)))


(defun audio-looped-p (source)
  (al:get-source (handle-value-of source) :looped))


(defun (setf audio-looped-p) (value source)
  (al:source (handle-value-of source) :looping (if value :true :false)))


(defun audio-gain (source)
  (al:get-source (handle-value-of source) :gain))


(defun (setf audio-gain) (value source)
  (al:source (handle-value-of source) :gain value))
