(in-package :cl-bodge.resources)

(defun decompose-path (path)
  (labels ((%decompose-path (parent-list path)
             (let ((parent (fad:pathname-parent-directory path)))
               (if (equal parent path)
                   parent-list
                   (%decompose-path (cons (enough-namestring path parent) parent-list)
                                    parent)))))
    (%decompose-path (unless (fad:directory-pathname-p path)
                       (list (file-namestring path)))
                     (fad:pathname-directory-pathname path))))


;;;
;;; Resource storage nodes
;;;
(defgeneric mount-resource-node (storage path attachable))
(defgeneric open-resource-stream (storage path))

(defmacro with-resource-stream ((stream path storage) &body body)
  (once-only (storage)
    `(let ((,stream (open-resource-stream ,storage ,path)))
       (unwind-protect
            (progn ,@body)
         (close ,stream)))))


(defclass path-node ()
  ((name :initarg :name :initform (error ":name missing") :reader name-of)
   (parent :initform nil :reader parent-of)
   (children :initform (make-hash-table :test 'equal))))


(defun %find-child (node name)
  (with-slots (children) node
    (gethash name children)))


(defun find-leaf-node (root path-list)
  (if-let ((child (%find-child root (first path-list))))
    (find-leaf-node child (rest path-list))
    (values root path-list)))


(defun attach-node (parent child)
  (setf (slot-value child 'parent) parent
        (gethash (name-of child) (slot-value parent 'children)) child))


(defun detach-node (parent child)
  (setf (slot-value child 'parent) nil)
  (remhash (name-of child) (slot-value parent 'children)))


(defun replace-node (source-node destination-node)
  (let ((parent (parent-of source-node)))
    (detach-node parent source-node)
    (attach-node parent destination-node)))


(defun %ensure-path (node path)
  (multiple-value-bind (leaf rest-path)
      (find-leaf-node node path)
    (loop with node = leaf
       for name in rest-path
       do (let ((child (make-instance 'path-node :name name)))
            (attach-node node child)
            (setf node child))
       finally (return node))))


(defmethod mount-resource-node ((this path-node) (path cons) (node path-node))
  (replace-node (%ensure-path this path) node))


(defmethod open-resource-stream ((this path-node) (path cons))
  (when-let ((child (%find-child this (first path))))
    (open-resource-stream child (rest path))))


(defclass filesystem-node (path-node)
  ((root-path :initarg :root-path :initform (error ":root-path missing"))))


(defmethod open-resource-stream ((this filesystem-node) (path cons))
  (with-slots (root-path) this
    (if-let ((stream (call-next-method)))
      stream
      (open (fad:merge-pathnames-as-file root-path (format nil "~{~A~}" path))
            :element-type '(unsigned-byte 8)))))


(defun make-filesystem-resource-provider (filesystem-path)
  (lambda (node-name)
    (make-instance 'filesystem-node :name node-name :root-path filesystem-path)))



;;;
;;; Resource storage
;;;
(defclass resource-storage ()
  ((root-node :initform (make-instance 'path-node :name "/"))))


(defmethod open-resource-stream ((this resource-storage) path)
  (with-slots (root-node) this
    (open-resource-stream root-node (decompose-path path))))


(defun mount-storage-resource-node (storage path node)
  (with-slots (root-node) storage
    (mount-resource-node root-node (decompose-path path) node)))
