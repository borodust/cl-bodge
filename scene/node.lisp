(in-package :cl-bodge.scene)

;;;
;;;
;;;
(defclass parent ()
  ((children :initform '() :reader children-of)))


(defgeneric adopt (child parent)
  (:method (child (this parent))
    (with-slots (children) this
      (nconcf children (list child)))))


(defmacro dochildren ((var parent) &body body)
  `(dolist (,var (children-of ,parent))
     ,@body))

;;;
;;;
;;;
(defclass node (parent)
  ((name :initarg :name :initform nil :reader name-of)))


(defgeneric simulation-pass (node)
  (:method ((this node))
    (dochildren (child this)
      (simulation-pass child))))


(defgeneric rendering-pass (node)
  (:method((this node))
    (dochildren (child this)
      (rendering-pass child))))


(labels ((%find-nodes (root name)
           (loop for child in (children-of root)
              when (eq (name-of child) name) append (list child)
              append (%find-nodes child name))))
  (defun find-node (root name)
    (let ((result (%find-nodes root name)))
      (if (eq (name-of root) name)
          (values root result)
          (values (first result) (rest result))))))


(defmacro %parse-node (node-def)
  (destructuring-bind (ctor-def &rest children) node-def
    (destructuring-bind (class &rest plist) (if (listp ctor-def)
                                                ctor-def
                                                (list ctor-def))
      `(let ((node (make-instance ',class ,@plist)))
         ,@(loop for child-def in children collecting
                `(adopt (%parse-node ,child-def) node))
         node))))
;;;
;;;
;;;
(defmacro scenegraph (root)
  `(%parse-node ,root))
