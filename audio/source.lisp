(in-package :cl-bodge.audio)


(defclass audio-source (al-object) ()
  (:default-initargs :id (al:gen-source)))


(defun make-audio-source ()
  (make-instance 'audio-source))


(declaim (ftype (function (audio-buffer audio-source) *) attach-buffer)
         (inline attach-buffer))
(defun attach-audio-buffer (buffer source)
  (al:source (id-of source) :buffer (id-of buffer)))


(declaim (ftype (function (audio-source) *) play-audio)
         (inline play-audio))
(defun play-audio (source)
  (al:source-play (id-of source)))


(declaim (ftype (function (audio-source) *) stop-audio)
         (inline stop-audio))
(defun stop-audio (source)
  (al:source-stop (id-of source)))


(declaim (ftype (function (audio-source) *) pause-audio)
         (inline pause-audio))
(defun pause-audio (source)
  (al:source-pause (id-of source)))
