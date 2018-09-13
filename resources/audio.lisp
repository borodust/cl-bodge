(cl:in-package :cl-bodge.resources)


(deftype pcm-data ()
  '(or (simple-array (unsigned-byte 8) (*))
    (simple-array (signed-byte 16) (*))
    (simple-array (signed-byte 32) (*))
    (simple-array single-float (*))
    (simple-array double-float (*))))


(deftype sample-depth ()
  '(member 8 16))


(defenum channel-format
  :mono :stereo)


(defgeneric audio->foreign-array (resource))
(defgeneric audio-channel-format (resource))
(defgeneric audio-sample-depth (resource))
(defgeneric audio-sampling-rate (resource))


(defclass audio ()
  ((channel-format :initarg :channel-format :initform nil
                   :type channel-format :reader audio-channel-format)
   (sample-depth :initarg :sample-depth :initform nil
                 :type sample-depth :reader audio-sample-depth)
   (sampling-rate :initarg :sampling-rate :initform nil
                  :type positive-integer :reader audio-sampling-rate)))


(defclass pcm-16-audio (audio) ()
  (:default-initargs :sample-depth 16))


(defclass cached-pcm-16-audio (pcm-16-audio)
  ((samples :initarg :samples :reader audio->foreign-array)))


(defun make-cached-pcm-16-audio (file)
  (make-instance 'cached-pcm-16-audio
                 :samples (let ((samples (sndfile:read-short-samples-into-array file)))
                            (make-foreign-array (length samples) :element-type '(signed-byte 16)
                                                                 :initial-contents samples))
                 :sampling-rate (sndfile:sound-sample-rate file)
                 :channel-format (ecase (sndfile:sound-channels file)
                                   (1 :mono)
                                   (2 :stereo))))

(defun load-ogg-vorbis-audio (path)
  (sndfile:with-open-sound-file (file path)
    (make-cached-pcm-16-audio file)))


;;;
;;; Audio resource
;;;

(defclass audio-resource-handler (resource-handler) ()
  (:default-initargs :resource-type :audio))


(defmethod decode-resource ((this audio-resource-handler) stream)
  (sndfile:with-sound-file-from-stream (file stream)
    (make-cached-pcm-16-audio file)))


(defmethod encode-resource ((this audio-resource-handler) (audio cached-pcm-16-audio) stream)
  (sndfile:write-short-samples-into-stream stream (simple-array-of (audio->foreign-array audio))
                                           :format :flac
                                           :channels (ecase (audio-channel-format audio)
                                                       (:mono 1)
                                                       (:stereo 2))
                                           :sample-rate (audio-sampling-rate audio)))


(defmethod make-resource-handler ((type (eql :audio)) &key)
  (make-instance 'audio-resource-handler))
