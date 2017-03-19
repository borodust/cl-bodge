(in-package :cl-bodge.interactions)


(defclass interactive-board-plane (plane-geom) ())


(defclass interactive-board-node (scene-node)
  ((windows :initarg :windows :initform nil)
   (origin :initform (vec4 0.0 0.0 0.0 0.0))
   (normal :initform (vec3 0.0 0.0 1.0))
   (x-axis :initform (vec3 1.0 0.0 0.0))
   (geom :initform nil)
   (poiu :initform nil)
   (y-axis :initform (vec3 0.0 1.0 0.0))

   (cursor-listener :initform nil)
   (mouse-listener :initform nil)
   (character-listener :initform nil)
   (input-state :initform (make-input-state))))


(defmethod discard-node :before ((this interactive-board-node))
  (with-slots (poiu windows) this
    (dolist (win windows)
      (dispose win))
    (dispose poiu)))


(defmethod initialize-instance :after ((this interactive-board-node) &key)
  (with-slots (y-axis x-axis normal) this
    (setf y-axis (cross normal x-axis))))


(defmethod initialization-flow ((this interactive-board-node)
                                &key width height font)
  (with-slots (geom poiu normal) this
    (>> (call-next-method)
        (~> (-> ((graphics)) ()
                  (setf poiu (make-poiu-context width height font 24 :antialiased-p t)))
            (-> ((physics)) ()
              (setf geom (make-instance 'interactive-board-plane :normal normal)))))))


(defun process-user-input (state poiu)
  (multiple-value-bind (x y) (read-cursor-state state)
    (with-poiu-input (poiu)
      (register-cursor-position x y)
      (loop for character in (read-characters state)
         do (register-character-input character))
      #++
      (if (read-backspace-click state)
          (register-keyboard-input :backspace :pressed)
          (register-keyboard-input :backspace :released))
      (register-mouse-input x y :left (read-mouse-state state :left))
      (register-mouse-input x y :right (read-mouse-state state :right)))))


(defmethod scene-pass ((this interactive-board-node) (pass rendering-pass) input)
  (with-slots (poiu windows input-state) this
    (clear-poiu-context poiu)
    (process-user-input input-state poiu)
    (dolist (win windows)
      (compose-poiu win poiu))
    (render poiu)))


(defun make-board-window (board-node x y w h &rest opts &key &allow-other-keys)
  (with-slots (poiu windows) board-node
    (let ((win (apply #'make-window poiu x y w h opts)))
      (push win windows)
      win)))


(defun update-board-canvas-size (board width height)
  (with-slots (poiu canvas-texture) board
    (ge.poiu::update-poiu-canvas-size poiu width height)))


(defun enable-cursor-input (interactive-node)
  (with-slots (cursor-listener input-state) interactive-node
    (unless cursor-listener
      (setf cursor-listener (subscribe-body-to (cursor-event (x y)) (events)
                              (register-cursor-state input-state x y))))))


(defun enable-mouse-input (interactive-node)
  (with-slots (mouse-listener input-state) interactive-node
    (unless mouse-listener
      (setf mouse-listener (subscribe-body-to (mouse-event (button state)) (events)
                             (register-mouse-state input-state button state))))))


(defun enable-character-input (interactive-node)
  (with-slots (character-listener input-state) interactive-node
    (unless character-listener
      (setf character-listener
            (subscribe-body-to (character-input-event (character)) (events)
              (register-character input-state character))))))
