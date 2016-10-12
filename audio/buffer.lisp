(in-package :cl-bodge.audio)


(deftype pcm-data ()
  '(or (simple-array (unsigned-byte 8) (*))
    (simple-array (signed-byte 16) (*))))


(deftype sample-depth ()
  '(member 8 16))


(defenum channel-format
  :mono :stereo)


(defclass audio-buffer (al-object) ()
  (:default-initargs :id (al:gen-buffer)))


(define-destructor audio-buffer ((id id-of) (sys system-of))
  (-> sys
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


(declaim (ftype (function (* pcm-data channel-format sample-depth integer) *) make-audio-buffer)
         (inline make-audio-buffer))
(defun make-audio-buffer (system pcm-data channel-format sample-depth sampling-rate)
  (make-instance 'audio-buffer
                 :system system
                 :channel-format channel-format
                 :sample-depth sample-depth
                 :sampling-rate sampling-rate
                 :pcm-data pcm-data))
