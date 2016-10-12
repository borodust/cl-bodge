(in-package :cl-bodge.graphics)


(defenum primitive
  :point :line :triangle-strip
  :lines-adjacency :triangles-adjacency)


(defclass mesh (disposable)
  ((vertex-array :initarg :vertex-array)
   (primitive-type :initarg :primitive-type :type primitive)))


(define-destructor mesh (vertex-array)
  (dispose vertex-array))


(defun make-mesh (vertex-array primitive-type)
  (make-instance 'mesh
                 :vertex-array vertex-array
                 :primitive-type primitive-type))


(defmethod render ((this mesh))
  (with-slots (vertex-array primitive-type) this
    (with-bound-vertex-array (vertex-array)
      (gl:draw-arrays primitive-type 0 (vertex-count-of vertex-array)))))


(defclass indexed-mesh (mesh)
  ((index-buffer :initarg :index-buffer)))


(defun make-indexed-mesh (vertex-array index-buffer primitive-type)
  (make-instance 'indexed-mesh
                 :vertex-array vertex-array
                 :primitive-type primitive-type
                 :index-buffer index-buffer))


(defmethod render ((this indexed-mesh))
  (with-slots (vertex-array primitive-type index-buffer) this
    (with-bound-vertex-array (vertex-array)
      (with-bound-buffer (index-buffer)
        (gl:draw-elements primitive-type (gl:make-null-gl-array :uint)
                          :count (index-count-of index-buffer))))))
