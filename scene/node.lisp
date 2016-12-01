(in-package :cl-bodge.scene)

;;;
;;;
;;;
(defclass node (parent)
  ((name :initarg :name :initform nil :reader name-of)
   (parent :initform nil)))


(defgeneric parent-of (node)
  (:method ((child node))
    (with-slots (parent) child
      (unless (null parent)
        (tg:weak-pointer-value parent)))))


(defgeneric node-attaching (this-node new-node)
  (:method ((this node) (new node))
    (when-let ((parent (parent-of this)))
      (node-attaching parent new))))


(defgeneric node-detached (this-node removed-node)
  (:method ((this node) (removed node))
    (when-let ((parent (parent-of this)))
      (node-detached parent removed))))


(defmethod adopt :before ((parent node) (child node))
  (when-let ((prev-parent (parent-of child)))
    (abandon parent child))
  (with-slots ((child-parent parent)) child
    (setf child-parent (tg:make-weak-pointer parent)))
  (node-attaching parent child))


(defmethod abandon :after ((parent node) (child node))
  (with-slots ((child-parent parent)) child
    (setf child-parent nil))
  (node-detached parent child))


(labels ((%find-nodes (root name)
           (loop for child in (children-of root)
              when (eq (name-of child) name) append (list child)
              append (%find-nodes child name))))
  (defun find-node (root name)
    (let ((result (%find-nodes root name)))
      (if (eq (name-of root) name)
          (values root result)
          (values (first result) (rest result))))))
