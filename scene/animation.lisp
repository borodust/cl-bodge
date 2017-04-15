(in-package :cl-bodge.scene)


;;;
;;;
;;;
(declaim (special *animation-frame*
                  *skeletons*))

(defclass animation-node (scene-node)
  ((animation :initarg :frames :initform (error ":frames initarg missing"))))


(defmethod scene-pass ((this animation-node) (pass rendering-pass) input)
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


(defclass animated-skeleton-node (scene-node)
  ((root :initarg :root-bone :initform (error ":root-bone initarg missing") :reader root-bone-of)
   (bones :initform (make-hash-table :test 'equal))))


(defmethod initialize-instance :after ((this animated-skeleton-node) &key)
  (with-slots (root bones) this
    (labels ((%flatten (node)
               (unless (null node)
                 (with-hash-entries ((n (name-of node))) bones
                   (unless (null n)
                     (error "Bone '~a' duplicate found" (name-of node)))
                   (setf n node))
                 (dochildren (child node)
                   (%flatten child)))))
      (%flatten root))))


(defun bone-transform (bone-name)
  ;; todo: cache results
  (labels ((%transform-for (bone)
             (if (null bone)
                 (identity-mat4)
                 (let ((parent (%transform-for (parent-of bone))))
                   (if-let ((animated (when-bound *animation-frame*
                                        (frame-transform-of *animation-frame* (name-of bone)))))
                     (mult parent animated)
                     (mult parent (transform-of bone)))))))
    (with-slots (bones) (first *skeletons*)
      (if-let ((bone (gethash bone-name bones)))
        (%transform-for bone)
        (if-let ((*skeletons* (rest *skeletons*)))
          (bone-transform bone-name)
          (error "Bone '~a' missing" bone-name))))))


(defmethod scene-pass ((this animated-skeleton-node) (pass rendering-pass) input)
  (let ((*skeletons* (if-bound *skeletons* (push this *skeletons*) (list this))))
    (unwind-protect
         (call-next-method)
      (pop *skeletons*))))
