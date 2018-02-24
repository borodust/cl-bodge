(cl:in-package :cl-bodge.audio)


(defhandle audio-buffer-handle
  :initform (claw:c-with ((buffer-id %al:uint))
              (%al:gen-buffers 1 (buffer-id &))
              buffer-id)
  :closeform (claw:c-with ((buffer-id %al:uint :value *handle-value*))
               (%al:delete-buffers 1 (buffer-id &))))


(defclass audio-buffer (al-object) ()
  (:default-initargs :handle (make-audio-buffer-handle)))


(defgeneric select-pcm-format (channel-format sample-depth)
  (:method (c s) (error "Unsupported PCM format")))

(defmethod select-pcm-format ((channel-format (eql :stereo)) (sample-depth (eql 16)))
  %al:+format-stereo16+)

(defmethod select-pcm-format ((channel-format (eql :stereo)) (sample-depth (eql 8)))
  %al:+format-stereo8+)

(defmethod select-pcm-format ((channel-format (eql :mono)) (sample-depth (eql 8)))
  %al:+format-mono8+)

(defmethod select-pcm-format ((channel-format (eql :mono)) (sample-depth (eql 16)))
  %al:+format-mono16+)


(defmethod initialize-instance :after ((this audio-buffer)
                                       &key channel-format sampling-rate sample-depth pcm-data)
  (let ((foreign-type (ecase sample-depth
                        (8 :uint8)
                        (16 :int16)))
        (foreign-size (* (/ sample-depth 8) (length pcm-data))))
    ;; fixme: remove :: hax
    (cffi::with-foreign-array (raw-data pcm-data (list :array foreign-type (length pcm-data)))
      (%al:buffer-data (handle-value-of this)
                       (select-pcm-format channel-format sample-depth)
                       raw-data foreign-size sampling-rate))))


(defun make-audio-buffer (resource)
  (make-instance 'audio-buffer
                 :channel-format (audio-channel-format-of resource)
                 :sample-depth (audio-sample-depth-of resource)
                 :sampling-rate (audio-sampling-rate-of resource)
                 :pcm-data (pcm-audio-data-of resource)))
