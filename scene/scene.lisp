(in-package :cl-bodge.scene)


(declaim (special
          *scene*
          *projection-matrix*
          *transform-matrix*
          *shading-pipeline*
          *shading-parameters*))

;;;
;;;
;;;
(defclass shading-parameters ()
  ((bindings :initform (make-hash-table :test #'equal))))


(defun bind-parameter (name shading-program)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (if (null programs)
          (setf programs (list shading-program))
          (push shading-program programs)))))


(defun unbind-parameter (name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (if (null programs)
          (error "No bindings found for parameter '~a'" name))
          (let ((prog-list programs))
            (if (null (rest prog-list))
                (remhash name bindings)
                (setf programs (rest prog-list)))))))


(defmacro with-bound-parameters ((parameters program) &body body)
  (once-only (parameters program)
    `(progn
       (dolist (name ,parameters)
         (bind-parameter name ,program))
       (unwind-protect
            (progn
              ,@body)
         (dolist (name ,parameters)
           (unbind-parameter name))))))


(defun (setf shading-parameter) (value name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (when (null programs)
        (error "Parameter with name '~a' is unbound" name))
      (setf (program-uniform-variable (first programs) name) value))))


(defun shading-parameter (name)
  (with-slots (bindings) *shading-parameters*
    (with-hash-entries ((programs name)) bindings
      (when (null programs)
        (error "Parameter with name '~a' is unbound" name))
      (program-uniform-variable (first programs) name))))

;;;
;;;
;;;
(defclass parent ()
  ((children :initform '() :reader children-of)))


(defgeneric adopt (child parent)
  (:method (child (this parent))
    (with-slots (children) this
      (nconcf children (list child)))))


(defmacro dochildren ((var parent) &body body)
  `(dolist (,var (children-of ,parent))
     ,@body))

;;;
;;;
;;;
(defclass node (parent)
  ((name :initarg :name :initform nil :reader name-of)))


(defgeneric simulate (node)
  (:method ((this node))
    (dochildren (child this)
      (simulate child))))


(defmethod render ((this node))
  (dochildren (child this)
    (render child)))


(labels ((%find-nodes (root name)
           (loop for child in (children-of root)
              when (eq (name-of child) name) append (list child)
              append (%find-nodes child name))))
  (defun find-node (root name)
    (let ((result (%find-nodes root name)))
      (if (eq (name-of root) name)
          (values root result)
          (values (first result) (rest result))))))

;;;
;;;
;;;
(defclass scene (node)
  ((gx :initarg :graphics-system :reader graphics-system-of)
   (phx :initarg :physics-system :reader physics-system-of)
   (host :initarg :host-system :reader host-system-of)))


(defmethod initialize-instance :after ((this scene) &key)
  ;; not quite thread safe
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
                  (simulate child)))
              (-> (graphics-system-of scene)
                (gl:clear :color-buffer :depth-buffer)
                (let ((*transform-matrix* (identity-mat4)))
                  (doscene (child scene)
                    (render child)))
                (swap-buffers (host-system-of scene))))))

;;;
;;;
;;;
(defclass body-transform-node (node)
  ((position :initform (make-vec3))
   (rotation :initform (identity-mat4))
   (body :initarg :body)))


(defmethod simulate :after ((this body-transform-node))
  (with-slots (position rotation body) this
    (setf position (position-of body)
          rotation (rotation-of body))))


(defmethod render ((this body-transform-node))
  (with-slots (position rotation) this
    (let ((*transform-matrix* (m* *transform-matrix*
                                  (translation-mat4 position)
                                  rotation)))
      (call-next-method))))

;;;
;;;
;;;
(defclass shading-pipeline-node (node)
  ((pipeline :initarg :pipeline)))


(defmethod render ((this shading-pipeline-node))
  (with-slots (pipeline) this
    (let ((old-pipeline (if-unbound *shading-pipeline* nil))
          (*shading-pipeline* pipeline)
          (*shading-parameters* (make-instance 'shading-parameters)))
      (with-bound-shading-pipeline (*shading-pipeline* old-pipeline)
        (call-next-method)))))

;;;
;;;
;;;
(defclass lighting-node (shading-node) ())


; defclass shadowing-node ?


;;;
;;;
;;;
(defclass texture-node (node)
  ((tex :initarg :texture)
   (unit :initarg :unit :initform 0)))


(defmethod render ((this texture-node))
  (with-slots (tex unit) this
    (with-bound-texture (tex unit)
      (call-next-method))))


;;;
;;;
;;;
(defclass mesh-node (node)
  ((mesh :initarg :mesh)))


(defmethod render ((this mesh-node))
  (with-slots (mesh) this
    (render mesh)
    (call-next-method)))

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


(defmethod render ((this projection-node))
  (with-slots (proj-mat) this
    (let ((*projection-matrix* proj-mat))
      (call-next-method))))

;;;
;;;
;;;
(defclass camera-node (node)
  ((camera-mat :initform (identity-mat4))))


(defmethod render ((this camera-node))
  (with-slots (camera-mat) this
    (let ((*transform-matrix* (m* *transform-matrix* camera-mat)))
      (call-next-method))))


(defun translate-camera (camera-node x y z)
  (with-slots (camera-mat) camera-node
    (setf camera-mat (m* (translation-mat4* (- x) (- y) (- z)) camera-mat))))


(defun rotate-camera (camera-node x y z)
  (with-slots (camera-mat) camera-node
    (setf camera-mat (m* (rotation-mat4* (- x) (- y) (- z)) camera-mat))))


;;;
;;;
;;;
(defclass shading-program-node (node)
  ((program :initarg :program)
   (parameters :initarg :parameters)))


(defmethod render ((this shading-program-node))
  (with-slots (program parameters) this
    (use-shading-program-stages *shading-pipeline* program :all-shader)
    (with-bound-parameters (parameters program)
      (call-next-method))))

;;;
;;;
;;;
(defclass shading-parameters-node (node)
  ((params :initarg :parameters)))


(defmethod render ((this shading-parameters-node))
  (with-slots (params) this
    (loop for (name . value) in params do
         (setf (shading-parameter name) (if (functionp value) (funcall value) value)))
    (call-next-method)))


;;;
;;;
;;;
(defclass transform-node (node)
  ((transform-mat :initarg :matrix :initform (identity-mat4))))


(defmethod render ((this transform-node))
  (with-slots (transform-mat) this
    (let ((*transform-matrix* (m* *transform-matrix*
                                  transform-mat)))
      (call-next-method))))

;;;
;;;
;;;
(defmacro parse-node (node-def)
  (destructuring-bind (ctor-def &rest children) node-def
    (destructuring-bind (class &rest plist) (if (listp ctor-def)
                                                ctor-def
                                                (list ctor-def))
      `(let ((node (make-instance ',class ,@plist)))
         ,@(loop for child-def in children collecting
                `(adopt (parse-node ,child-def) node))
         node))))

(defmacro scenegraph (root)
  `(parse-node ,root))



;;;
