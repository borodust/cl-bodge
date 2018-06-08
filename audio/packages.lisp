(ge.util:define-package :cl-bodge.audio
  (:use :cl :cl-bodge.engine :cl-bodge.utils :cl-muth)
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
           audio-pitch

           make-audio-buffer
           attach-audio-buffer))
