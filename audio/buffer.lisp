(in-package :cl-bodge.audio)


(defhandle audio-buffer-handle
    :initform (al:gen-buffer)
    :closeform (al:delete-buffer *handle-value*))


(defclass audio-buffer (al-object) ()
  (:default-initargs :handle (make-audio-buffer-handle)))


(defmethod initialize-instance :after ((this audio-buffer)
                                       &key channel-format sampling-rate sample-depth pcm-data)
  (let ((pcm-format (make-keyword (format nil "~a~a" channel-format sample-depth)))
        (foreign-type (ecase sample-depth
                        (8 :uint8)
                        (16 :int16)))
        (foreign-size (* (/ sample-depth 8) (length pcm-data))))
    (cffi:with-foreign-array (raw-data pcm-data (list :array foreign-type (length pcm-data)))
      (al:buffer-data (handle-value-of this) pcm-format raw-data foreign-size sampling-rate))))



(define-system-function make-audio-buffer audio-system (resource &key (system *system*))
  (make-instance 'audio-buffer
                 :system system
                 :channel-format (audio-channel-format-of resource)
                 :sample-depth (audio-sample-depth-of resource)
                 :sampling-rate (audio-sampling-rate-of resource)
                 :pcm-data (pcm-audio-data-of resource)))
