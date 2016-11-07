(in-package :cl-bodge.scene)


(declaim (special
          *projection-matrix*))



;;;
;;;
;;;
(defclass projection-node (node)
  ((proj-mat :initform nil)))


(defun update-projection (projection-node w h)
  (setf (slot-value projection-node 'proj-mat)
        (if (> w h)
            (perspective-projection-mat #f2 #f(* 2 (/ h w)) #f2 #f1000)
            (perspective-projection-mat #f(* 2 (/ w h)) #f2 #f2 #f1000))))


(defmethod initialize-instance :after ((this projection-node) &key ((:width w)) ((:height h)))
  (update-projection this w h))


(defmethod rendering-pass ((this projection-node))
  (with-slots (proj-mat) this
    (let ((*projection-matrix* proj-mat))
      (call-next-method))))


;;;
;;;
;;;
(defclass transform-node (node)
  ((mat :initform (identity-mat4))))


(defgeneric translate-node (node x y z)
  (:method ((this transform-node) x y z)
    (with-slots (mat) this
      (setf mat (mult (translation-mat4 #f x #f y #f z) mat)))))


(defgeneric rotate-node (node x y z)
  (:method ((this transform-node) x y z)
    (with-slots (mat) this
      (setf mat (mult (euler-angles->mat4 (vec3 #f x #f y #f z)) mat)))))


(defmethod rendering-pass ((this transform-node))
  (with-slots (mat) this
    (let ((*transform-matrix* (mult *transform-matrix* mat)))
      (call-next-method))))
;;;
;;;
;;;
(defclass camera-node (node)
  ((camera-mat :initform (identity-mat4))))


(defmethod rendering-pass ((this camera-node))
  (with-slots (camera-mat) this
    (let ((*transform-matrix* (mult *transform-matrix* camera-mat)))
      (call-next-method))))


(defgeneric translate-camera (camera-node x y z)
  (:method ((this camera-node) x y z)
    (with-slots (camera-mat) this
      (setf camera-mat (mult (translation-mat4 #f(- x) #f(- y) #f(- z)) camera-mat)))))


(defgeneric rotate-camera (camera-node x y z)
  (:method ((this camera-node) x y z)
    (with-slots (camera-mat) this
      (setf camera-mat (mult (euler-angles->mat4 (vec3 #f(- x) #f(- y) #f(- z))) camera-mat)))))

;;;
;;;
;;;
(defclass body-transform-node (node)
  ((position :initform (vec3))
   (rotation :initform (identity-mat4))
   (body :initarg :body)))


(defmethod simulation-pass :after ((this body-transform-node))
  (with-slots (position rotation body) this
    (setf position (position-of body)
          rotation (rotation-of body))))


(defmethod rendering-pass ((this body-transform-node))
  (with-slots (position rotation) this
    (let ((*transform-matrix* (mult *transform-matrix*
                                    (vec->translation-mat4 position)
                                    rotation)))
      (call-next-method))))
