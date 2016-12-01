(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.audio
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.audio.resources
        :cl :cl-muth)
  (:nicknames :ge.snd)
  (:export audio-system
           audio

           listener-gain
           listener-position
           listener-velocity
           listener-orientation

           make-audio-source
           play-audio
           pause-audio
           stop-audio
           audio-gain
           audio-looped-p

           make-audio-buffer
           attach-audio-buffer))
