(in-package :cl-bodge.scene)


(declaim (special *projection-matrix*
                  *model-matrix*
                  *view-matrix*))

(defun model-view-projection-matrix ()
  (mult *projection-matrix*
        *view-matrix*
        *model-matrix*))
;;;
;;;
;;;
(defclass projection-node (scene-node)
  ((perspective-p :initform t :initarg :perspective-p)
    (proj-mat :initform nil)))


(defmethod initialize-instance :after ((this projection-node)
                                       &key (width 2.0)
                                         (aspect (error ":aspect missing"))
                                         (near 2.0)
                                         (far 1000.0))
  (update-projection this aspect :width width :near near :far far))


(defun update-projection (projection-node aspect &key (width 2.0) (near 2.0) (far 1000.0))
  (with-slots (proj-mat perspective-p) projection-node
    (setf proj-mat (if perspective-p
                       (perspective-projection-mat (f width)
                                                   (f (/ width aspect))
                                                   (f near)
                                                   (f far))
                       (orthographic-projection-mat (f width)
                                                    (f (/ width aspect))
                                                    (f near)
                                                    (f far))))))


(defmethod scene-pass ((this projection-node) pass input)
  (with-slots (proj-mat) this
    (let ((*projection-matrix* proj-mat))
      (call-next-method))))


;;;
;;;
;;;
(defclass camera-node (scene-node)
  ((transform :initform (identity-mat4) :accessor transform-of :initarg :transform)))


(defmethod scene-pass ((this camera-node) pass input)
  (let ((*view-matrix* (transform-of this)))
    (call-next-method)))


;;;
;;;
;;;
(defclass transform-node (scene-node)
  ((mat :initarg :transform :initform (identity-mat4))))



(defmethod initialize-instance :after ((this transform-node) &key (translation nil t-p)
                                                               (rotation nil r-p)
                                                               (scaling nil s-p))
  (let ((transform (identity-mat4)))
    (when t-p
      (setf transform (vec->translation-mat4 translation)))
    (when r-p
      (setf transform (mult (euler-angles->mat4 rotation) transform)))
    (when s-p
      (setf transform (mult (vec->scaling-mat4 scaling) transform)))
    (with-slots (mat) this
      (setf mat transform))))


(defgeneric translate-node (node x y z)
  (:method ((this transform-node) x y z)
    (with-slots (mat) this
      (setf mat (mult (translation-mat4 #f x #f y #f z) mat)))))


(defgeneric rotate-node (node x y z)
  (:method ((this transform-node) x y z)
    (with-slots (mat) this
      (setf mat (mult (euler-angles->mat4 (vec3 #f x #f y #f z)) mat)))))


(defmethod scene-pass ((this transform-node) pass input)
  (with-slots (mat) this
    (let ((*model-matrix* (if-bound *model-matrix*
                                    (mult *model-matrix* mat)
                                    mat)))
      (call-next-method))))
