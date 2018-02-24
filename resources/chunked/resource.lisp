(cl:in-package :cl-bodge.resources)


(declaim (special *objects*
                  *resource-path*
                  *resolvers*))


(defun push-object (id obj)
  (setf (gethash id *objects*) obj))


(defgeneric parse-chunk (chunk-type parameters data)
  (:method (chunk-type parameters data)
    (error "Unknown chunk type: ~a" chunk-type)))


(defgeneric read-chunk (chunk-type parameters stream)
  (:method (chunk-type parameters stream)
    (parse-chunk chunk-type parameters
                 (read-preserving-whitespace
                  (flexi-streams:make-flexi-stream stream :external-format :utf-8)))))


(defgeneric write-chunk (chunk stream))


(defmethod read-chunk :around (chunk-type parameters stream)
  (flet ((resolve-references (resolvers)
           (dolist (fn resolvers)
             (funcall fn))))
    (let* ((*objects* (make-hash-table :test 'equal))
           (*resolvers* '()))
      (prog1 (call-next-method)
        (resolve-references *resolvers*)))))

;;;
;;; BRF container resource
;;;

(defstruct chunk-record
  (type nil :type keyword :read-only t)
  (parameters nil :type cons :read-only t)
  (position 0 :type fixnum :read-only t)
  (size 0 :type fixnum :read-only t))


(defclass resource ()
  ((chunks :initarg :chunks)
   (path :initarg :path :reader path-of)))


(defun find-chunk (resource chunk-name)
  (with-slots (chunks) resource
    (gethash chunk-name chunks)))


(defun list-chunks (resource)
  (with-slots (chunks) resource
    (loop for chunk being the hash-value of chunks using (hash-key name)
         collect (cons name chunk))))


(defun read-deflated (in chunk-type parameters)
  (destructuring-bind (&key compression &allow-other-keys) parameters
    (if compression
        (ecase compression
          (:deflate
           (destructuring-bind (&key (compressed-size (error ":compressed-size missing"))
                                     &allow-other-keys)
               parameters
             (let* ((start-position (file-position in))
                    (inflated (chipz:make-decompressing-stream 'chipz:deflate in)))
               (prog1 (read-chunk chunk-type parameters inflated)
                 ;; fixme: dirty hack? mb better to allocate buffer from source stream
                 (file-position in (+ start-position compressed-size)))))))
        (read-chunk chunk-type parameters in))))


(defun load-container (path)
  (with-open-file (in (fad:canonical-pathname path) :element-type '(unsigned-byte 8))
    (let ((char-stream (flexi-streams:make-flexi-stream in :external-format :utf-8)))
      (destructuring-bind (format version) (read char-stream)
        (unless (eq :brf format)
          (error "Unknown format: ~a" format))
        (unless (eql 1 version)
          (error "Unsupported version: ~a" version))
        (let ((chunk-table (make-hash-table :test 'equal)))
          (loop for chunk-header = (read-preserving-whitespace char-stream nil nil nil)
             for position = (file-position in)
             until (null chunk-header)
             do (destructuring-bind (chunk-type &rest parameters &key name size
                                                &allow-other-keys)
                    chunk-header
                  (with-hash-entries ((chunk name)) chunk-table
                    (cond
                      ((null name)
                       (error "Nameless chunk header of type ~A found" chunk-type))
                      ((null size)
                       (error "Size for '~A' chunk is not specified" name))
                      ((not (null chunk))
                       (error "Duplicate chunk with name '~A' found" name)))
                    (setf chunk (make-chunk-record :type chunk-type
                                                   :parameters parameters
                                                   :position position
                                                   :size size)))
                  (file-position in (+ position size))))
          (make-instance 'resource :chunks chunk-table :path path))))))
