(in-package :cl-bodge.scene)


(declaim (special *current-node*
                  *current-scene-pass*))


(defgeneric invoke-scene-pass (scene-pass continuation)
  (:method (scene-pass continuation)
    (funcall continuation)))


(defun process-node-children ()
  (loop for child in (children-of *current-node*)
        do (let ((*current-node* child))
             (setf (%pass-input-of child) (process-scene-node (%value-of child)
                                                              *current-scene-pass*
                                                              (%pass-input-of child))))))


(defgeneric process-scene-node (node pass input)
  (:method (node pass input)
    (declare (ignore node pass))
    (process-node-children)
    input))


(defgeneric discard-scene-node (node)
  (:method (node) (declare (ignore node))))



(defclass scene-node (parent)
  ((value :initarg :value :reader %value-of)
   (pass-input :initform nil :accessor %pass-input-of)))

;;;
;;;
;;;
(defclass scene (disposable)
  ((pass-chain :initarg :pass-chain)
   (redefined-p :initform nil)
   (root :initform (make-instance 'scene-node) :reader %root-of)))


(defun make-scene (scene-class &rest pass-chain)
  (make-instance scene-class :pass-chain pass-chain))


(defun discard-tree (root)
  (dotree (node root :post)
    (discard-scene-node node)))


(define-destructor scene (root)
  (discard-tree root))


(defmacro doscene ((child scene) &body body)
  (once-only (scene)
    `(dotree (,child (%root-of ,scene))
       ,@body)))


(defmacro scene-tree ((parent) &body children)
  `(parent-tree (,parent)
     ,@children))
;;;
;;;
;;;
(defgeneric reinitialize-scene (scene)
  (:method (scene) (declare (ignore scene))))


(defgeneric %reinitialize-scene (scene))


(defun %make-scene-node (value-class &rest args &key &allow-other-keys)
  (make-instance 'scene-node :value (apply #'make-instance value-class args)))


(defmacro defscene (name-and-opts &body nodes)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (with-gensyms (this)
      `(progn
         (defclass ,name (scene ,@(assoc-value opts :inherit)) ())
         (defmethod %reinitialize-scene ((,this ,name))
           (descard-tree (%root-of ,this))
           (parent-tree (,this #'%make-scene-node) ,@nodes)
           (reinitialize-scene ,this))
         (make-instances-obsolete ',name)))))


(defmethod initialize-instance :after ((this scene) &key)
  (reinitialize-scene this))


(defmethod update-instance-for-redefined-class :after ((this scene)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (redefined-p) this
    (setf redefined-p t)))


(defun process-scene (scene scene-pass)
  (let ((*current-node* (%root-of scene))
        (*current-scene-pass* scene-pass))
    (process-node-children)))


(defun impel-scene (scene)
  (with-slots (pass-chain redefined-p) scene
    (when redefined-p
      (%reinitialize-scene scene)
      (setf redefined-p nil))
    (let ((chain pass-chain))
      (labels ((%process-scene ()
                 (process-scene scene (first chain))
                 (setf chain (rest chain))
                 (when chain
                   (invoke-scene-pass (first chain) #'%process-scene))))
        (invoke-scene-pass (first chain) #'%process-scene)))))
