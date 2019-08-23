(cl:in-package :cl-bodge.audio)


(defhandle audio-buffer-handle
  :initform (claw:c-with ((buffer-id %al:uint))
              (%al:gen-buffers 1 (buffer-id &))
              buffer-id)
  :closeform (claw:c-with ((buffer-id %al:uint :value *handle-value*))
               (%al:delete-buffers 1 (buffer-id &))))


(defclass audio-buffer (al-object) ()
  (:default-initargs :handle (make-audio-buffer-handle)))


(defun select-pcm-format (channel-format sample-depth)
  (cond
    ((and (eq channel-format :stereo) (= sample-depth 16)) %al:+format-stereo16+)
    ((and (eq channel-format :stero) (= sample-depth 8)) %al:+format-stereo8+)
    ((and (eq channel-format :mono) (= sample-depth 16)) %al:+format-mono16+)
    ((and (eq channel-format :mono) (= sample-depth 8)) %al:+format-mono8+)
    (t (error "Unsupported PCM format"))))


(defmethod initialize-instance :after ((this audio-buffer)
                                       &key channel-format sampling-rate sample-depth pcm-data)
  (when pcm-data
   (assert (or
            (and (= sample-depth 8)
                 (or (subtypep (type-of pcm-data) '(simple-array (unsigned-byte 8)))
                     (subtypep (type-of pcm-data) '(simple-array (signed-byte 8)))))
            (and (= sample-depth 16)
                 (or (subtypep (type-of pcm-data) '(simple-array (unsigned-byte 16)))
                     (subtypep (type-of pcm-data) '(simple-array (signed-byte 16)))))))
   (let ((foreign-size (* (/ sample-depth 8) (length pcm-data))))

     (with-simple-array-pointer (ptr pcm-data)
       (%al:buffer-data (handle-value-of this)
                        (select-pcm-format channel-format sample-depth)
                        ptr foreign-size sampling-rate)))))


(defun make-audio-buffer (resource)
  (make-instance 'audio-buffer
                 :channel-format (ge.rsc:audio-channel-format resource)
                 :sample-depth (ge.rsc:audio-sample-depth resource)
                 :sampling-rate (ge.rsc:audio-sampling-rate resource)
                 :pcm-data (simple-array-of (ge.rsc:audio->foreign-array resource))))


(defun make-empty-audio-buffer (&key (channel-format +default-channel-format+)
                                  (sample-depth +default-sample-depth+)
                                  (sampling-rate +default-sampling-rate+))
  (make-instance 'audio-buffer
                 :channel-format channel-format
                 :sample-depth sample-depth
                 :sampling-rate sampling-rate))
