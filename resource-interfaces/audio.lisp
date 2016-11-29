(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.audio.resources
  (:nicknames :ge.snd.rsc)
  (:use :cl-bodge.utils
        :cl)
  (:export pcm-data
           sample-depth
           channel-format

           pcm-audio-data-of
           audio-channel-format-of
           audio-sample-depth-of
           audio-sampling-rate-of))



(in-package :cl-bodge.audio.resources)


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
