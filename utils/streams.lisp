(in-package :cl-bodge.utils)


(defun stream->byte-array (stream &key (initial-size 4096))
  (check-type initial-size positive-integer)
  (do ((buffer (make-array initial-size :element-type (stream-element-type stream)))
       (offset 0)
       (offset-wanted 0))
      ((/= offset-wanted offset)
       (if (= offset (length buffer))
           buffer
           (subseq buffer 0 offset)))
    (unless (zerop offset)
      (let ((new-buffer (make-array (* 2 (length buffer))
                                    :element-type (stream-element-type stream))))
        (replace new-buffer buffer)
        (setf buffer new-buffer)))
    (setf offset-wanted (length buffer)
          offset (read-sequence buffer stream :start offset))))


(declaim (inline file->byte-array))
(defun file->byte-array (pathname &optional (element-type '(unsigned-byte 8)))
  (with-input-from-file (stream pathname :element-type element-type)
    (stream->byte-array stream)))


;;;
;;; Bounded input stream
;;;
(defclass bounded-input-stream (fundamental-input-stream)
  ((stream :initarg :stream :initform (error ":stream missing"))
   (position :initform 0 :initarg :position :type fixnum)
   (bound :initform -1 :initarg :bound :type fixnum)))


(defun make-bounded-input-stream (stream bound)
  (make-instance 'bounded-input-stream
                 :stream stream
                 :bound bound))


(defmethod stream-file-position ((this bounded-input-stream))
  (with-slots (position) this
    position))


(defmethod (setf stream-file-position) ((value fixnum) (this bounded-input-stream))
  (with-slots (position bound) this
    (cond
      ((< value 0)
       (error "Stream position must be non-negative fixnum"))
      ((> value bound)
       (error "Position ~A is out of bound, max ~A expected" value bound)))
    (setf position value)))


(defmethod stream-peek-char ((this bounded-input-stream))
  (with-slots (stream) this
    (peek-char nil stream)))


(labels ((increment-position (stream requested-size)
           (with-slots (position bound) stream
             (let* ((available-size (min (- bound position) requested-size)))
               (prog1 available-size
                 (incf position available-size)))))
         (funcall-or-eof (this reader)
           (with-slots (stream) this
             (let ((available-size (increment-position this 1)))
               (if (= available-size 0)
                   :EOF
                   (funcall reader stream))))))

  (defmethod stream-read-sequence ((this bounded-input-stream) sequence start end &key)
    (with-slots (stream) this
      (read-sequence sequence stream
                     :start start
                     :end (+ start (increment-position this (- end start))))))

  (defmethod stream-read-byte ((this bounded-input-stream))
    (funcall-or-eof this #'read-byte))

  (defmethod stream-read-char ((this bounded-input-stream))
    (funcall-or-eof this #'read-char))

  (defmethod stream-read-char-no-hang ((this bounded-input-stream))
    (funcall-or-eof this #'read-char-no-hang))

  (defmethod stream-read-line ((this bounded-input-stream))
    (error "read-line is not implemented for bounded-input-stream")))
