(cl:in-package :cl-bodge.resources)


(define-constant +brf-magic+ "bodged!"
  :test #'equal)


;;;
;;; BRF resource container
;;;
(defstruct chunk-record
  (type nil :type keyword :read-only t)
  (compression nil :type (or null keyword) :read-only t)
  (position 0 :type fixnum :read-only t)
  (size 0 :type fixnum :read-only t))


(defclass resource-container ()
  ((chunks :initarg :chunks)
   (path :initarg :path :reader path-of)))


(defun find-chunk (resource chunk-name)
  (with-slots (chunks) resource
    (gethash chunk-name chunks)))


(defun list-chunks (resource)
  (with-slots (chunks) resource
    (loop for chunk being the hash-value of chunks using (hash-key name)
         collect (cons name chunk))))


(defun validate-magic (stream)
  (let ((buffer (make-array (length +brf-magic+) :element-type (array-element-type +brf-magic+)
                                                 :initial-element #\Space)))
    (read-sequence buffer stream)
    (unless (equal +brf-magic+ buffer)
      (error "Invalid BRF file"))))


(defun validate-version (stream)
  (let ((version (read-byte stream)))
    (unless (= 2 version)
      (error "Unsupported version: ~a" version))))


(defun write-chunk (out type name bytes &key compression (start 0) end)
  (unless (subtypep (array-element-type bytes) '(unsigned-byte 8))
    (error "Byte array must be of '(unsigned-byte 8) type"))
  (let ((out (flex:make-flexi-stream out :external-format :utf-8)))
    (with-standard-io-syntax
      (let ((*print-pretty* nil)
            (*print-readably* t)
            (length (max (- (or end (length bytes)) start) 0)))
        (prin1 (nconc (list type :name name
                                 :size length)
                      (when compression
                        (list compression)))
               out)))
    (write-sequence bytes out :start start :end end)))


(defun load-container (path)
  (with-open-file (in (fad:canonical-pathname path) :element-type '(unsigned-byte 8))
    (let ((char-stream (flexi-streams:make-flexi-stream in :external-format :utf-8)))
      (validate-magic char-stream)
      (validate-version in)
      (with-standard-io-syntax
        (let ((chunk-table (make-hash-table :test 'equal))
              (*read-eval* nil))
          (loop for chunk-header = (read-preserving-whitespace char-stream nil nil nil)
                for position = (file-position in)
                until (null chunk-header)
                do (destructuring-bind (chunk-type &key name size compression) chunk-header
                     (with-hash-entries ((chunk name)) chunk-table
                       (cond
                         ((null name)
                          (error "Nameless chunk header of type ~A found" chunk-type))
                         ((null size)
                          (error "Size for '~A' chunk is not specified" name))
                         ((not (null chunk))
                          (error "Duplicate chunk with name '~A' found" name)))
                       (setf chunk (make-chunk-record :type chunk-type
                                                      :compression compression
                                                      :position position
                                                      :size size)))
                     (file-position in (+ position size))))
          (make-instance 'resource-container :chunks chunk-table :path path))))))
