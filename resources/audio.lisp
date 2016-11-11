(in-package :cl-bodge.resources)


(defclass audio ()
  ((channel-format :initarg :channel-format :initform nil
                   :type channel-format :reader audio-channel-format-of)
   (sample-depth :initarg :sample-depth :initform nil
                 :type sample-depth :reader audio-sample-depth-of)
   (sampling-rate :initarg :sampling-rate :initform nil
                  :type positive-integer :reader audio-sampling-rate-of)))


(defclass pcm-16-audio (audio)
  ((path :initarg :path))
  (:default-initargs :sample-depth 16))


(defun load-ogg-vorbis-audio (path)
  (with-open-sound-file (file path)
    (make-instance 'pcm-16-audio
                   :path path
                   :sampling-rate (sound-sample-rate file)
                   :channel-format (ecase (sound-channels file)
                                     (1 :mono)
                                     (2 :stereo)))))


(defmethod pcm-audio-data-of ((this pcm-16-audio))
  (with-slots (path) this
    (with-open-sound-file (file path)
      (read-short-samples-into-array file))))
