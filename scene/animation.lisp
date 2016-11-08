(in-package :cl-bodge.scene)


;;;
;;;
;;;
(declaim (special *animation-frame*
                  *skeleton*))

(defclass animation-node (node)
  ((animation :initarg :frames :initform (error ":frames initarg missing"))))


(defmethod rendering-pass ((this animation-node))
  (with-slots (animation) this
    (let ((*animation-frame* (frame-at animation)))
      (call-next-method))))


(defun start-node-animation (animated-bone-node)
  (unless (null animated-bone-node)
    (with-slots (animation) animated-bone-node
      (start-animation animation))))


(defun reset-node-animation (animated-bone-node)
  (unless (null animated-bone-node)
    (with-slots (animation) animated-bone-node
      (reset-animation animation))))


;;;
;;;
;;;
(defclass bone-node (node)
  ((transform :initarg :transform :initform (identity-mat4) :reader transform-of)))


(defclass animated-skeleton-node (node)
  ((root :initarg :root-bone :initform (error ":root-bone initarg missing") :reader root-bone-of)
   (bones :initform (make-hash-table :test 'equal))))


(defmethod initialize-instance :after ((this animated-skeleton-node) &key)
  (with-slots (root bones) this
    (labels ((%flatten (node)
               (with-hash-entries ((n (name-of node))) bones
                 (unless (null n)
                   (error "Bone '~a' duplicate found" (name-of node)))
                 (setf n node))
               (dochildren (child node)
                 (%flatten child))))
      (%flatten root))))


(defun bone-transform (bone-name)
  ;; todo: cache results
  (labels ((%transform-for (bone)
             (if (null bone)
                 (identity-mat4)
                 (let ((parent (%transform-for (parent-of bone))))
                   (if-let ((animated (frame-transform-of *animation-frame* (name-of bone))))
                     (mult parent animated)
                     (mult parent (transform-of bone)))))))
    (with-slots (bones) *skeleton*
      (if-let ((bone (gethash bone-name bones)))
        (%transform-for bone)
        (error "Bone '~a' missing" bone-name)))))


(defmethod rendering-pass ((this animated-skeleton-node))
  (let ((*skeleton* this))
    (call-next-method)))
