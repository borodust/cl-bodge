(in-package :cl-bodge.scene)


;;;
;;;
;;;
(declaim (special *animation-frame*))

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
(declaim (special *bones*))

(defclass skeleton-node (node) ())


(defmethod rendering-pass ((this skeleton-node))
  (let ((*bones* '()))
    (call-next-method)))


;;;
;;;
;;;
(defclass bone-node (node)
  ((rot :initform nil :reader rotation-of)
   (transl :initform nil :reader translation-of)))


(defmethod initialize-instance :after ((this bone-node) &key translation rotation)
  (with-slots (rot transl) this
    (setf transl (if (null translation)
                     (identity-mat4)
                     (sequence->translation-mat4 translation))
          rot (if (null rotation)
                  (identity-mat4)
                  (euler-angles->mat4 (sequence->vec3 rotation))))))


(defgeneric mat-of (bone-node)
  (:method ((this bone-node))
    (mult (translation-of this) (rotation-of this))))


(defmethod rendering-pass ((this bone-node))
  (with-accessors ((mat mat-of)) this
    (let ((bone (if (null *bones*) mat (mult (first *bones*) mat))))
      (push bone *bones*)
      (unwind-protect
           (call-next-method)
        (pop *bones*)))))


(defun rotate-bone (bone x y z)
  (with-slots (rot) bone
    (setf rot (mult (euler-angles->mat4 (vec3 x y z)) rot))))


(defun translate-bone (bone x y z)
  (with-slots (transl) bone
    (setf transl (mult (translation-mat4 x y z) transl))))


;;;
;;;
;;;
(defclass bone-to-world-transform-node (node) ())


(defmethod rendering-pass ((this bone-to-world-transform-node))
  (let ((*transform-matrix* (mult *transform-matrix* (first *bones*))))
    (call-next-method)))

;;;
;;;
;;;
(defclass animated-bone-node (bone-node)
  ((seq-id :initarg :sequence :initform (error ":sequence initarg missing"))))


(defmethod rotation-of ((this animated-bone-node))
  (with-slots (seq-id) this
    (if-let ((mat (frame-rotation-of *animation-frame* seq-id)))
      (mult mat (call-next-method))
      (call-next-method))))
