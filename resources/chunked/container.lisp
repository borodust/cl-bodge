(cl:in-package :cl-bodge.resources)


;;;
;;; BRF container node
;;;

(defclass container-node (path-node)
  ((container)
   (root-path :initarg :root-path :initform #p"/")))


(defmethod initialize-instance :after ((this container-node) &key container-path)
  (with-slots (container) this
    (setf container (load-container container-path))))


(defun ensure-inflated (stream compression)
  (if compression
      (let ((format (ecase compression
                      (:deflate 'chipz:deflate)
                      (:zlib 'chipz:zlib)
                      (:gzip 'chipz:zlib))))
        (chipz:make-decompressing-stream format stream))
      stream))


(defun container-resource-type (container path)
  (when-let ((record (find-chunk container (namestring path))))
    (chunk-record-type record)))


(defmethod resource-type ((this container-node) (path null))
  (with-slots (root-path container) this
    (container-resource-type container #p"/")))


(defmethod resource-type ((this container-node) (path cons))
  (with-slots (container root-path) this
    (if-let ((type (call-next-method)))
      type
      (let ((name (apply #'fad:merge-pathnames-as-file "/" path)))
        (container-resource-type container name)))))


(defun open-bound-chunk-stream (container path)
  (when-let ((record (find-chunk container (fad:merge-pathnames-as-file "/" (namestring path)))))
    (let ((stream (open (path-of container) :element-type '(unsigned-byte 8))))
      (file-position stream (chunk-record-position record))
      (make-bounded-input-stream (ensure-inflated stream (chunk-record-compression record))
                                 (chunk-record-size record)))))


(defmethod open-resource-stream ((this container-node) (path null))
  (with-slots (root-path container) this
    (open-bound-chunk-stream container root-path)))


(defmethod open-resource-stream ((this container-node) (path cons))
  (with-slots (container root-path) this
    (if-let ((stream (call-next-method)))
      stream
      (let ((name (apply #'fad:merge-pathnames-as-file root-path path)))
        (open-bound-chunk-stream container name)))))


(defun make-container-resource-provider (container-path &optional (root-path "/"))
  (lambda (node-name)
    (make-instance 'container-node
                   :name node-name
                   :container-path container-path
                   :root-path root-path)))


(defun mount-container (resource-path container-path &optional (container-root "/"))
  (mount-resource-provider resource-path (make-container-resource-provider container-path
                                                                           container-root)))
