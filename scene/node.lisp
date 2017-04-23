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


(defun find-node (root name)
  (if (eq (name-of root) name)
      root
      (loop for child in (children-of root)
         thereis (find-node child name))))
