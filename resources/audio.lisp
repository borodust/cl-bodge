(in-package :cl-bodge.resources)


(defclass audio ()
  ((channel-format :initarg :channel-format :initform nil
                   :type channel-format :reader audio-channel-format-of)
   (sample-depth :initarg :sample-depth :initform nil
                 :type sample-depth :reader audio-sample-depth-of)
   (sampling-rate :initarg :sampling-rate :initform nil
                  :type positive-integer :reader audio-sampling-rate-of)))


(defclass pcm-16-audio (audio) ()
  (:default-initargs :sample-depth 16))


(defclass cached-pcm-16-audio (pcm-16-audio)
  ((samples :initarg :samples :reader pcm-audio-data-of)))


(defun make-cached-pcm-16-audio (file)
  (make-instance 'cached-pcm-16-audio
                 :samples (bodge-sndfile:read-short-samples-into-array file)
                 :sampling-rate (bodge-sndfile:sound-sample-rate file)
                 :channel-format (ecase (bodge-sndfile:sound-channels file)
                                   (1 :mono)
                                   (2 :stereo))))

(defun load-ogg-vorbis-audio (path)
  (bodge-sndfile:with-open-sound-file (file path)
    (make-cached-pcm-16-audio file)))


;;;
;;; Audio resource
;;;

(defclass audio-resource-handler () ())


(defmethod decode-resource ((this audio-resource-handler) stream)
  (bodge-sndfile:with-sound-file-from-stream (file stream)
    (make-cached-pcm-16-audio file)))


(defmethod encode-resource ((this audio-resource-handler) (audio cached-pcm-16-audio) stream)
  (bodge-sndfile:write-short-samples-into-stream stream (pcm-audio-data-of audio)
                                                 :format :flac
                                                 :channels (ecase (audio-channel-format-of audio)
                                                             (:mono 1)
                                                             (:stereo 2))
                                                 :sample-rate (audio-sampling-rate-of audio)))


(defmethod make-resource-handler ((type (eql :audio)) &key)
  (make-instance 'audio-resource-handler))
