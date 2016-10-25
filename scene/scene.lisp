(in-package :cl-bodge.scene)


(declaim (special
          *scene*
          *transform-matrix*))


(defclass scene-root-node (node)
  ((gx :initform (engine-system 'graphics-system) :reader graphics-system-of)
   (phx :initform (engine-system 'physics-system) :reader physics-system-of)
   (host :initform (engine-system 'host-system) :reader host-system-of)))

;;;
;;;
;;;
(defclass scene (disposable)
  ((root :initform (make-instance 'scene-root-node) :reader root-of)))


(defun make-scene (&rest children)
  (let* ((scene (make-instance 'scene))
         (root (root-of scene)))
    (dolist (child children)
      (adopt root child))
    scene))


(defgeneric node-enabled-p (node)
  (:method (node) t))


(defgeneric initialize-node (node system)
  (:method (node system)))


(defgeneric discard-node (node)
  (:method (node)))


(defun discard-tree (root)
  (dotree (node root :post)
    (discard-node node)))


(define-destructor scene (root)
  (discard-tree root))


(defmethod initialize-instance :after ((this scene) &key)
  ;; not quite thread-safe
  (-> (graphics-system-of (root-of this))
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:enable :blend
               :depth-test)
    (gl:blend-func :src-alpha :one-minus-src-alpha)))


(definline node (scene name)
  (find-node (root-of scene) name))


(defgeneric simulation-pass (node)
  (:method ((this node))
    (dochildren (child this)
      (when (node-enabled-p child)
        (simulation-pass child)))))


(defgeneric rendering-pass (node)
  (:method ((this node))
    (dochildren (child this)
      (when (node-enabled-p child)
        (rendering-pass child)))))


(defmacro doscene ((child scene) &body body)
  (once-only (scene)
    `(let ((*scene* ,scene))
       (dotree (,child (root-of ,scene))
         ,@body))))


(defun animate (scene)
  (when-all* ((-> (physics-system-of (root-of scene))
                (simulation-pass (root-of scene)))
              (-> (graphics-system-of (root-of scene))
                (gl:clear :color-buffer :depth-buffer)
                (let ((*transform-matrix* (identity-mat4)))
                  (rendering-pass (root-of scene)))
                (swap-buffers (host-system-of (root-of scene)))))))


(defmethod node-attaching :after ((this scene-root-node) (that node))
  (let ((gx-sys (graphics-system-of this))
        (phx-sys (physics-system-of this)))
    (-> gx-sys
      (dotree (node that)
        (initialize-node node gx-sys)))
    (-> phx-sys
      (dotree (node that)
        (initialize-node node phx-sys)))))


(defmethod node-detached :after ((this scene-root-node) (that node))
  (discard-tree that))
