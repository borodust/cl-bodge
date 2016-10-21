(in-package :cl-bodge.scene)


(declaim (special
          *scene*
          *transform-matrix*))


;;;
;;;
;;;
(defclass scene (node)
  ((gx :initarg :graphics-system :reader graphics-system-of)
   (phx :initarg :physics-system :reader physics-system-of)
   (host :initarg :host-system :reader host-system-of)))


(defmethod initialize-instance :after ((this scene) &key)
  ;; not quite thread-safe
  (-> (graphics-system-of this)
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:enable :blend
               :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)))



(defmacro doscene ((child scene) &body body)
  (once-only (scene)
    `(let ((*scene* ,scene))
       (dochildren (,child ,scene)
         ,@body))))


(defun animate (scene)
  (when-all* ((-> (physics-system-of scene)
                (doscene (child scene)
                  (simulation-pass child)))
              (-> (graphics-system-of scene)
                (gl:clear :color-buffer :depth-buffer)
                (let ((*transform-matrix* (identity-mat4)))
                  (doscene (child scene)
                    (rendering-pass child)))
                (swap-buffers (host-system-of scene))))))
