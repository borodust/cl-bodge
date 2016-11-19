(in-package :cl-bodge.scene)


(declaim (special
          *scene*
          *transform-matrix*))


(defclass scene-root-node (node)
  ((gx :initform (engine-system 'graphics-system) :reader graphics-system-of)
   (phx :initform (engine-system 'physics-system) :reader physics-system-of)
   (au :initform (engine-system 'audio-system) :reader audio-system-of)
   (host :initform (engine-system 'host-system) :reader host-system-of)))

;;;
;;;
;;;
(defclass scene (disposable)
  ((root :initform (make-instance 'scene-root-node) :reader root-of)))


(defgeneric node-enabled-p (node)
  (:method (node) t))


(defgeneric initialize-node (node system)
  (:method (node system)))


(defgeneric discard-node (node)
  (:method (node)))


(defun discard-tree (root)
  (dotree (node root :post)
    (discard-node node)))


(defun initialize-tree (scene root)
  (let* ((scene-root (root-of scene))
         (gx-sys (graphics-system-of scene-root))
         (phx-sys (physics-system-of scene-root))
         (au-sys (audio-system-of scene-root)))
    (when-all ((-> (gx-sys :high)
                 (dotree (node root)
                   (initialize-node node gx-sys)))
               (-> (phx-sys :high)
                 (dotree (node root)
                   (initialize-node node phx-sys)))
               (-> (au-sys :high)
                 (dotree (node root)
                   (initialize-node node au-sys)))))))


(defun make-scene (&rest children)
  (let* ((scene (make-instance 'scene))
         (root (root-of scene)))
    (dolist (child children)
      (adopt root child))
    (initialize-tree scene root)
    scene))


(define-destructor scene (root)
  (discard-tree root))


(defmethod initialize-instance :after ((this scene) &key)
  ;; not quite thread-safe
  (-> ((graphics-system-of (root-of this)))
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
  (when-all* ((-> ((physics-system-of (root-of scene)))
                (simulation-pass (root-of scene)))
              (-> ((graphics-system-of (root-of scene)))
                (gl:clear :color-buffer :depth-buffer)
                (let ((*transform-matrix* (identity-mat4)))
                  (rendering-pass (root-of scene)))
                (swap-buffers (host-system-of (root-of scene)))))))
