(in-package :cl-bodge.audio)




(defclass audio-buffer (al-object) ()
  (:default-initargs :id (al:gen-buffer)))


(define-destructor audio-buffer ((id id-of) (sys system-of))
  (-> (sys)
    (al:delete-buffer id)))


(defmethod initialize-instance :after ((this audio-buffer)
                                       &key channel-format sampling-rate sample-depth pcm-data)
  (let ((pcm-format (make-keyword (format nil "~a~a" channel-format sample-depth)))
        (foreign-type (ecase sample-depth
                        (8 :uint8)
                        (16 :int16)))
        (foreign-size (* (/ sample-depth 8) (length pcm-data))))
    (cffi:with-foreign-array (raw-data pcm-data (list :array foreign-type (length pcm-data)))
      (al:buffer-data (id-of this) pcm-format raw-data foreign-size sampling-rate))))



(definline make-audio-buffer (system resource)
  (make-instance 'audio-buffer
                 :system system
                 :channel-format (audio-channel-format-of resource)
                 :sample-depth (audio-sample-depth-of resource)
                 :sampling-rate (audio-sampling-rate-of resource)
                 :pcm-data (pcm-audio-data-of resource)))
