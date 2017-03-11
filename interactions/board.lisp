(in-package :cl-bodge.interactions)


(defclass interactive-board-plane (plane-geom) ())


(defclass interactive-board-node (scene-node)
  ((windows :initarg :windows :initform nil)
   (origin :initform (vec4 0.0 0.0 0.0 0.0))
   (normal :initform (vec3 0.0 0.0 1.0))
   (x-axis :initform (vec3 1.0 0.0 0.0))
   (geom :initform nil)
   (poiu :initform nil)
   (canvas-texture :initform nil)
   (y-axis :initform (vec3 0.0 1.0 0.0))))


(defmethod discard-node :before ((this interactive-board-node))
  (with-slots (poiu canvas-texture windows) this
    (dolist (win windows)
      (dispose win))
    (dispose poiu)
    (dispose canvas-texture)))


(defmethod initialize-instance :after ((this interactive-board-node) &key)
  (with-slots (y-axis x-axis normal) this
    (setf y-axis (cross normal x-axis))))


(defmethod initialization-flow ((this interactive-board-node)
                                &key width height font)
  (with-slots (geom poiu canvas-texture normal) this
    (>> (call-next-method)
        (~> (>> (-> ((graphics)) ()
                  (setf canvas-texture (make-2d-texture (make-blank-image width height)
                                                        :rgba :generate-mipmaps-p nil)
                        poiu (make-poiu-context width height font 24 :antialiased-p t))))
            (-> ((physics)) ()
              (setf geom (make-instance 'interactive-board-plane :normal normal)))))))


(defmethod scene-pass ((this interactive-board-node) (pass rendering-pass) input)
  (with-slots (canvas-texture banner poiu windows) this
    (clear-poiu-context poiu)
    (dolist (win windows)
      (compose-poiu win poiu))
    (render-poiu poiu canvas-texture)
    (let ((*banner-texture* canvas-texture))
      (call-next-method))))


(defun make-board-window (board-node x y w h &rest opts &key &allow-other-keys)
  (with-slots (poiu windows) board-node
    (let ((win (apply #'make-window poiu x y w h opts)))
      (push win windows)
      win)))
