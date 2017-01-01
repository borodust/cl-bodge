(in-package :cl-bodge.scene)


(declaim (special *scene*
                  *transform-matrix*))


;;;
;;;
;;;
(defclass scene-node (node)
  ((pass-data :initform nil :accessor pass-data-of)))


(defgeneric node-enabled-p (node)
  (:method (node) t))


(defgeneric initialize-node (node system)
  (:method (node system)))


(defgeneric discard-node (node)
  (:method (node)))


(define-flow initialize-tree (root &rest systems)
  (flet ((initializer (system)
           (-> (system :priority :high :important-p t) ()
             (dotree (node root)
               (initialize-node node system)))))
    (~> (mapcar #'initializer systems))))



(defun discard-tree (root)
  (dotree (node root :post)
    (discard-node node)))


(defgeneric scene-pass (node pass input)
  (:method ((this scene-node) pass input)
    (dochildren (child this)
      (when (node-enabled-p child)
        (setf (pass-data-of child)
              (scene-pass child pass (pass-data-of child)))))
    input))

;;;
;;;
;;;
(defclass scene-pass () ())


(defgeneric run-scene-pass (pass node)
  (:method (pass node)
    (scene-pass node pass nil)))



(defmethod dispatch ((this scene-pass) (task function) &key)
  (funcall task)
  t)


(defclass system-scene-pass (dispatcher scene-pass)
  ((system :initarg :system :reader system-of)))


(defmethod dispatch ((this system-scene-pass) (task function) &rest keys &key)
  (apply #'dispatch (append (list (system-of this) task) keys)))


;;;
;;;
;;;
(defclass pass-chain ()
  ((passes :initarg :passes :reader passes-of)))


(definline make-pass-chain (&rest passes)
  (make-instance 'pass-chain :passes passes))


(define-flow process-pass-chain (chain root-node)
  (flet ((make-processor (pass)
           (-> (pass) ()
             (run-scene-pass pass root-node))))
    (mapcar #'make-processor (passes-of chain))))


;;;
;;;
;;;
(defclass scene-root-node (scene-node) ())


;;;
;;;
;;;
(defclass scene (disposable)
  ((pass-chain :initarg :pass-chain :initform (error "Pass chain must be supplied")
               :reader pass-chain-of)
   (root :initform (make-instance 'scene-root-node) :reader root-of)))


(defun make-scene (pass-chain &rest children)
  (let* ((scene (make-instance 'scene :pass-chain pass-chain))
         (root (root-of scene)))
    (dolist (child children)
      (adopt root child))
    scene))


(define-destructor scene (root)
  (discard-tree root))


(definline node (scene name)
  (find-node (root-of scene) name))


(defmacro doscene ((child scene) &body body)
  (once-only (scene)
    `(let ((*scene* ,scene))
       (dotree (,child (root-of ,scene))
         ,@body))))


(define-flow animate (scene)
  (process-pass-chain (pass-chain-of scene) (root-of scene)))


;;;
;;;
;;;
(defmacro %parse-node (node-def)
  (destructuring-bind (ctor-def &rest children) node-def
    (destructuring-bind (class &rest plist) (if (listp ctor-def)
                                                ctor-def
                                                (list ctor-def))
      `(let ((node (make-instance ',class ,@plist)))
         ,@(loop for child-def in children collecting
                `(adopt node (%parse-node ,child-def)))
         node))))

(defmacro scenegraph (root)
  `(%parse-node ,root))
