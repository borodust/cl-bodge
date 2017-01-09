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
  ((proj-mat :initform nil)))


(defun update-projection (projection-node w h)
  (setf (slot-value projection-node 'proj-mat)
        (if (> w h)
            (perspective-projection-mat #f2 #f(* 2 (/ h w)) #f2 #f1000)
            (perspective-projection-mat #f(* 2 (/ w h)) #f2 #f2 #f1000))))


(defmethod initialize-instance :after ((this projection-node) &key ((:width w)) ((:height h)))
  (update-projection this w h))


(defmethod scene-pass ((this projection-node) pass input)
  (with-slots (proj-mat) this
    (let ((*projection-matrix* proj-mat))
      (call-next-method))))


;;;
;;;
;;;
(defclass camera-node (scene-node) ())


(defgeneric camera-transform (camera-node))


(defmethod scene-pass ((this camera-node) pass input)
  (let ((*view-matrix* (camera-transform this)))
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


(defmethod scene-pass ((this transform-node) (pass rendering-pass) input)
  (with-slots (mat) this
    (let ((*model-matrix* (mult *model-matrix* mat)))
      (call-next-method))))
