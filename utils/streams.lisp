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
