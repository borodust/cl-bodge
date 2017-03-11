(in-package :cl-bodge.interactions)

(defclass ray-selectable () ())

(defclass control-ray-geom (ray-geom) ())


(defmethod filter-contacts (contacts (this control-ray-geom) that)
  nil)


(defmethod filter-contacts (contacts this (that control-ray-geom))
  (filter-contacts contacts that this))


(defmethod filter-contacts (contacts (this control-ray-geom) (that ray-selectable))
  nil)


(defmethod filter-contacts (contacts (that ray-selectable) (this control-ray-geom))
  (filter-contacts contacts this that))


(defmethod collide ((this control-ray-geom) (that ray-selectable))
  nil)


(defmethod collide ((this ray-selectable) (that control-ray-geom))
  (collide that this))


;;;
;;;
;;;
;;;
;;;
;;;
(defclass control-ray-node (scene-node)
  ((origin :initform (vec4 0.0 0.0 -1.0 1.0))
   (end :initform (vec4 0.0 0.0 1.0 1.0))
   (metrics :initform nil)
   (width :initform nil)
   (height :initform nil)
   (ray :initform nil)))


(defmethod initialize-instance :after ((this control-ray-node) &key ((:width w)) ((:height h)))
  (with-slots (width height metrics) this
    (setf width w
          height h)))


(defun update-ray-position (control-ray-node x y)
  (with-slots (origin end width height) control-ray-node
    (setf (x origin) #f(- (/ x width 0.5) 1.0)
          (y origin) #f(- (/ y height 0.5) 1.0)
          (x end) (x origin)
          (y end) (y origin))))


(defmethod initialize-node :after ((this control-ray-node) (system physics-system))
  (with-slots (ray) this
    (setf ray (make-instance 'control-ray
                             :direction (vec3 0.0 0.0 -1.0)
                             :length 1000.0))))


(defmethod scene-pass ((this control-ray-node) (pass simulation-pass) input)
  (with-slots (origin end ray metrics) this
    (let ((inverted (inverse (mult *projection-matrix* *view-matrix*))))
      (flet ((deperspectify (vec)
               (let* ((hg (mult inverted vec))
                      (w (w hg)))
                 (vec3 #f(/ (x hg) w) #f(/ (y hg) w) #f(/ (z hg) w)))))
        (let* ((t-pos (deperspectify origin))
               (b-pos (deperspectify end))
               (dir (subt b-pos t-pos)))
          (setf (position-of ray) t-pos
                (direction-of ray) dir)
          (call-next-method)
          (list :ray-position t-pos :ray-direction dir))))))


(declaim (special *control-ray-position*
                  *control-ray-direction*))


(defmethod scene-pass ((this control-ray-node) (pass rendering-pass) input)
  (destructuring-bind (&key ray-position ray-direction &allow-other-keys) input
    (let ((*control-ray-position* ray-position)
          (*control-ray-direction* ray-direction))
      (call-next-method))))
