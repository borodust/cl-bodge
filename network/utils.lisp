(in-package :cl-bodge.network)


(declaim (special *loop-handle*
                  *allocator*))

(define-constant +max-frame-size+ (* 64 1024))

;;;
;;;
;;;
(defclass circular-buffer ()
  ((buffer :initform nil)
   (start :initform 0)
   (end :initform 0)
   (empty-p :initform t)))


(defmethod initialize-instance :after ((this circular-buffer) &key (size (error ":size missing")))
  (with-slots (buffer) this
    (setf buffer (make-array size :element-type '(unsigned-byte 8)))))


(definline make-circular-buffer (size)
  (check-type size fixnum)
  (make-instance 'circular-buffer :size size))


(defun buffer-filled-size (buffer)
  (with-slots (start end buffer empty-p) buffer
    (cond
      (empty-p 0)
      ((> end start) (- end start))
      (t (+ (- (length buffer) start) end)))))


(definline buffer-size (buffer)
  (with-slots (buffer) buffer
    (length buffer)))


(defun fill-buffer (circular-buffer stream)
  (with-slots (buffer start end empty-p) circular-buffer
    (labels ((%update-end (bytes-read)
               (incf end bytes-read)
               (when (and (> bytes-read 0) empty-p)
                 (setf empty-p nil)))
             (%fill-buffer (circular-buffer stream read-before)
               (let* ((len (length buffer)))
                 (cond
                   ((and (not empty-p) (= start end)) (values read-before 0))
                   ((< end start)
                    (let ((bytes-read (- (read-sequence buffer stream
                                                        :start end
                                                        :end start)
                                         end)))
                      (%update-end bytes-read)
                      (values (+ read-before bytes-read)
                              (- start end))))

                   (t
                    (let ((bytes-read (- (read-sequence buffer stream :start end) end)))
                      (%update-end bytes-read)
                      (if (= end len)
                          (progn
                            (setf end 0)
                            (%fill-buffer circular-buffer stream bytes-read))
                          (values bytes-read
                                  (- end start)))))))))
      (%fill-buffer circular-buffer stream 0))))


(defun read-buffer (circular-buffer seq &key ((:start seq-start) 0) ((:end seq-end)))
  (with-slots (buffer start end empty-p) circular-buffer
    (labels ((%read-buffer (circular-buffer seq seq-start seq-end)
               (let* ((len (length buffer))
                      (seq-end (if seq-end (max seq-end seq-start) (length seq)))
                      (tail-size (if (> end start) (- end start) (- len start)))
                      (read-len (min (buffer-filled-size circular-buffer) (- seq-end seq-start))))
                 (cond
                   ((= read-len 0) 0)
                   ((<= read-len tail-size)
                    (let ((next-start (+ start read-len)))
                      (replace seq buffer
                               :start1 seq-start
                               :end1 seq-end
                               :start2 start
                               :end2 next-start)
                      (if (= next-start (length buffer))
                          (setf start 0)
                          (setf start next-start))
                      (when (= start end)
                        (setf start 0
                              end 0
                              empty-p t)))
                    read-len)
                   (t
                    (let ((seq-mid (+ seq-start tail-size)))
                      (+ (%read-buffer circular-buffer seq seq-start seq-mid)
                         (%read-buffer circular-buffer seq seq-mid seq-end))))))))
      (%read-buffer circular-buffer seq seq-start seq-end))))


;;;
;;;
;;;
(defclass circular-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((buffer :initform (error ":buffer missing")
           :initarg :buffer :reader buffer-of
           :type circular-buffer)))


;;;
;;;
;;;
(defclass buffered-output-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((buffer :initform (error ":buffer missing")
           :initarg :buffer :reader buffer-of
           :type (simple-array (unsigned-byte 8) (*)))
   (position :initform 0 :initarg :position :type fixnum)))


(definline make-buffered-output-stream (buffer &optional (position 0))
  (make-instance 'buffered-output-stream :buffer buffer :position position))


(defmethod trivial-gray-streams:stream-file-position ((this buffered-output-stream))
  (with-slots (position) this
    position))


(defmethod (setf trivial-gray-streams:stream-file-position) (value (this buffered-output-stream))
  (with-slots (buffer position) this
    (assert (and (>= value 0) (<= value (length buffer))) (value)
            "Provided position is out of bounds")
    (setf position value)))


(defmethod trivial-gray-streams:stream-write-sequence ((this buffered-output-stream)
                                                       seq start end &key)
  (assert (<= start end) (start end))
  (with-slots (buffer position) this
    (let ((new-pos (+ position (- end start))))
      (replace buffer seq
               :start1 position :end1 new-pos
               :start2 start :end2 end)
      (setf position (min new-pos (length buffer)))
      (when (> new-pos (length buffer))
        (signal (make-condition 'end-of-file :stream this)))
      seq)))


(defmethod trivial-gray-streams:stream-write-byte ((this buffered-output-stream) (byte fixnum))
  (with-slots (buffer position) this
    (when (= position (length buffer))
      (signal (make-condition 'end-of-file :stream this)))
    (setf (aref buffer position) byte)
    (incf position))
  byte)


;;;
;;; Object pool
;;;

(defclass object-pool ()
  ((ctor :initarg :constructor)
   (dtor :initarg :destructor)
   (available-objects :initform (list))
   (acquired-objects :initform (make-hash-table))))


(defun clear-object-pool (object-pool)
  (with-slots (available-objects acquired-objects dtor) object-pool
    (when dtor
      (loop for obj in available-objects
         do (funcall dtor obj)
         finally (setf available-objects (list)))
      (loop for obj being the hash-key of acquired-objects
         do (funcall dtor obj)
         finally (clrhash acquired-objects)))))


(definline make-object-pool (object-constructor &optional object-destructor)
  (make-instance 'object-pool :constructor object-constructor :destructor object-destructor))


(defun acquire-object (object-pool)
  (with-slots (available-objects acquired-objects ctor) object-pool
    (unless available-objects
      (push (funcall ctor) available-objects))
    (let ((obj (pop available-objects)))
      (setf (gethash obj acquired-objects) obj))))


(defun release-object (object-pool object)
  (with-slots (available-objects acquired-objects dtor) object-pool
    (when (gethash object acquired-objects)
      (push object available-objects)
      (remhash object acquired-objects))))
