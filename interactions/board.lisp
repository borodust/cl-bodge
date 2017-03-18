(in-package :cl-bodge.interactions)


(declaim (special *interactive-board-texture*))


(defclass interactive-board-plane (plane-geom) ())


(defclass interactive-board-node (scene-node)
  ((windows :initarg :windows :initform nil)
   (origin :initform (vec4 0.0 0.0 0.0 0.0))
   (normal :initform (vec3 0.0 0.0 1.0))
   (x-axis :initform (vec3 1.0 0.0 0.0))
   (geom :initform nil)
   (poiu :initform nil)
   (y-axis :initform (vec3 0.0 1.0 0.0))

   (framebuffer :initform nil :reader framebuffer-of)
   (depth-stencil-buffer :initform nil)
   (canvas-texture :initform nil)

   (cursor-callback :initform nil)
   (mouse-callback :initform nil)
   (input-state :initform (make-input-state))))


(defmethod discard-node :before ((this interactive-board-node))
  (with-slots (poiu canvas-texture windows framebuffer depth-stencil-buffer) this
    (dolist (win windows)
      (dispose win))
    (dispose poiu)
    (dispose framebuffer)
    (dispose canvas-texture)
    (dispose depth-stencil-buffer)))


(defmethod initialize-instance :after ((this interactive-board-node) &key)
  (with-slots (y-axis x-axis normal) this
    (setf y-axis (cross normal x-axis))))


(defmethod initialization-flow ((this interactive-board-node)
                                &key width height font)
  (with-slots (geom poiu canvas-texture normal framebuffer depth-stencil-buffer) this
    (>> (call-next-method)
        (~> (-> ((graphics)) ()
                  (setf canvas-texture (make-2d-texture (make-blank-image width height)
                                                        :rgba :generate-mipmaps-p nil)
                        poiu (make-poiu-context width height font 24 :antialiased-p t)
                        framebuffer (make-framebuffer)
                        depth-stencil-buffer (make-renderbuffer :depth-stencil width height))
                  (attach-depth-stencil-buffer framebuffer depth-stencil-buffer))
            (-> ((physics)) ()
              (setf geom (make-instance 'interactive-board-plane :normal normal)))))))


(defun process-user-input (state poiu)
  (multiple-value-bind (x y) (read-cursor-state state)
    (with-poiu-input (poiu)
      (register-cursor-position x y)
      (loop for character = (read-character state) while character
           do (register-character-input character))
      #++
      (if (read-backspace-click state)
          (register-keyboard-input :backspace :pressed)
          (register-keyboard-input :backspace :released))
      (register-mouse-input x y :left (read-mouse-state state :left))
      (register-mouse-input x y :right (read-mouse-state state :right)))))


(defun render-poiu (poiu)
  (preserving-state
    (gx.state:clear-color 0.0 0.0 0.0 0.5)
    (gx.state:clear-depth 1.0)
    (gl:clear :color-buffer :depth-buffer :stencil-buffer)
    (render poiu)))


(defmethod scene-pass ((this interactive-board-node) (pass rendering-pass) input)
  (with-slots (canvas-texture poiu windows input-state framebuffer) this
    (clear-poiu-context poiu)
    (process-user-input input-state poiu)
    (dolist (win windows)
      (compose-poiu win poiu))
    (with-complete-framebuffer (framebuffer :color-buffers canvas-texture)
      (render-poiu poiu))
    (let ((*interactive-board-texture* canvas-texture))
      (call-next-method))))


(defun make-board-window (board-node x y w h &rest opts &key &allow-other-keys)
  (with-slots (poiu windows) board-node
    (let ((win (apply #'make-window poiu x y w h opts)))
      (push win windows)
      win)))


(defun update-board-canvas-size (board width height)
  (with-slots (poiu) board
    (ge.poiu::update-poiu-canvas-size poiu width height)))


(defun enable-cursor-input (interactive-node)
  (with-slots (cursor-callback input-state) interactive-node
    (unless cursor-callback
      (setf cursor-callback (subscribe-body-to (cursor-event (x y)) (events)
                              (register-cursor-state input-state x y))))))


(defun enable-mouse-input (interactive-node)
  (with-slots (mouse-callback input-state) interactive-node
    (unless mouse-callback
      (setf mouse-callback (subscribe-body-to (mouse-event (button state)) (events)
                             (register-mouse-state input-state button state))))))