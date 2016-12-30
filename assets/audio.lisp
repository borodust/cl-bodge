(in-package :cl-bodge.assets)


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


(defgeneric pcm-audio-data-of (resource))
(defgeneric audio-channel-format-of (resource))
(defgeneric audio-sample-depth-of (resource))
(defgeneric audio-sampling-rate-of (resource))
