(cl:in-package :cl-bodge.audio)

(define-constant +default-channel-format+ :mono)

(define-constant +default-sample-depth+ 16)

(define-constant +default-sampling-rate+ 44100)


(defclass al-object (foreign-object) ())
