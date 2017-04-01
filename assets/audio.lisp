(in-package :cl-bodge.assets)


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


(defun load-ogg-vorbis-audio (path)
  (with-open-sound-file (file path)
    (make-instance 'cached-pcm-16-audio
                   :samples (read-short-samples-into-array file)
                   :sampling-rate (sound-sample-rate file)
                   :channel-format (ecase (sound-channels file)
                                     (1 :mono)
                                     (2 :stereo)))))
