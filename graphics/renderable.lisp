(in-package :cl-bodge.graphics)

(defgeneric render (renderable))


;;
;;
(defclass rendering-group ()
  ((renderables :initform '() :reader renderables-of)))


(defmethod render ((this rendering-group))
  (loop for renderable in (renderables-of this) collecting
       (render renderable)))

(defgeneric add-renderable (renderable rendering-group)
  (:method (renderable (rendering-group rendering-group))
    (with-slots (renderables) rendering-group
      (push renderable renderables))))
