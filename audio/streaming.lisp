(cl:in-package :cl-bodge.audio)


(defclass audio-source-stream (disposable fundamental-output-stream)
  ((source :initform nil)
   (channel-format :initarg :channel-format :initform (error ":channel-format missing"))
   (sample-depth  :initarg :sample-depth :initform (error ":sample-depth missing"))
   (sampling-rate :initarg :sampling-rate :initform (error ":sampling-rate missing"))
   (buffer-queue :initform (list nil))))


(defmethod initialize-instance :after ((this audio-source-stream) &key)
  (with-slots (source buffer-queue channel-format sampling-rate sample-depth) this
    (setf source (make-audio-source))
    (pushnew (make-empty-audio-buffer :channel-format channel-format
                                      :sampling-rate sampling-rate
                                      :sample-depth sample-depth)
             buffer-queue)))


(define-destructor audio-source-stream (source)
  (dispose source))


(defun make-audio-source-stream (&key (channel-format +default-channel-format+)
                                   (sampling-rate +default-sampling-rate+)
                                   (sample-depth +default-sample-depth+)
                                   sample-chunk-length)
  (flet ((%not-nil (value name)
           (or value (error "~A must not be nil" name))))
    (make-instance 'audio-source-stream
                   :channel-format (%not-nil channel-format ":channel-format")
                   :sampling-rate (%not-nil sampling-rate ":sampling-rate")
                   :sample-depth (%not-nil sample-depth ":sample-depth")
                   :sample-chunk-length (%not-nil sample-chunk-length
                                                  ":sample-chunk-length"))))


(definline check-audio-stream-open-p (this)
  (with-slots (source) this
    (when (disposedp source)
      (error "Audio stream closed"))))


(defmethod stream-write-sequence ((this audio-source-stream) sequence start end &key)
  (with-slots (source) this
    (check-audio-stream-open-p this)))


(defmethod stream-write-byte ((this audio-source-stream) integer)
  (with-slots (source) this
    (check-audio-stream-open-p this)))


(defmethod stream-clear-output ((this audio-source-stream))
  (with-slots (source) this
    (check-audio-stream-open-p this)))


(defmethod stream-finish-output ((this audio-source-stream))
  (clear-output this))


(defmethod stream-force-output ((this audio-source-stream))
  (clear-output this))


(defmethod close ((this audio-source-stream) &key &allow-other-keys)
  (unless (disposedp this)
    (clear-output this)
    (dispose this)))
