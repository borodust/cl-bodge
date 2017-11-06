(in-package :cl-bodge.assets)


;;;
;;; BRF container node
;;;

(defclass container-node (path-node)
  ((container)
   (root-path :initarg :root-path :initform #p"/")))


(defmethod initialize-instance :after ((this container-node) &key container-path)
  (with-slots (container) this
    (setf container (load-container container-path))))


(defun open-bound-chunk-stream (container path)
  (when-let ((record (find-chunk container (namestring path))))
    (let ((stream (open (path-of container) :element-type '(unsigned-byte 8))))
      (file-position stream (chunk-record-position record))
      (break "~A" record)
      (flex:make-flexi-stream stream
                              :element-type '(unsigned-byte 8)
                              :bound (chunk-record-size record)))))


(defmethod open-resource-stream ((this container-node) (path null))
  (with-slots (root-path container) this
    (open-bound-chunk-stream container root-path)))


(defmethod open-resource-stream ((this container-node) (path cons))
  (with-slots (container root-path) this
    (if-let ((stream (call-next-method)))
      stream
      (let ((name (fad:merge-pathnames-as-file root-path (format nil "~{~A~}" path))))
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
