(in-package :cl-bodge.engine)


(declaim (special *system*))

;;
(defclass bodge-engine ()
  ((systems :initform nil)
   (engine-lock :initform nil)
   (properties :initform '())
   (working-directory :initform nil :reader working-directory-of)
   (shared-executors :initform nil)
   (shared-pool :initform nil)
   (disabling-order :initform '())))

(defvar *engine* nil)


(definline engine ()
  *engine*)

(defun engine-system (system-name)
  (with-slots (systems engine-lock) (engine)
    (with-recursive-lock-held (engine-lock)
      (if-let ((system (gethash system-name systems)))
        system
        (error (format nil "~a not found" system-name))))))


(defun working-directory ()
  (working-directory-of (engine)))


(defun merge-working-pathname (pathname)
  (merge-pathnames pathname (working-directory)))


;;
(defclass system ()
  ((dependencies :initarg :depends-on :initform '() :reader dependencies-of)))


(defgeneric enable (system)
  (:method (system) nil))

(defgeneric disable (system)
  (:method (system) nil))

(defgeneric enabledp (system))

;;
(defun instantiate-systems (system-class-names &optional sys-alist)
  (loop for class-name in system-class-names
       with result = sys-alist
     unless (assoc class-name result)
     do (let ((system (make-instance (find-class class-name))))
          (setf result
                (instantiate-systems (dependencies-of system)
                                     (acons class-name system result))))
       finally (return result)))


(defun enable-system (system-class sys-table &optional order)
  (flet ((system-enabled-p (system-class)
           (member system-class order)))
    (let ((system (gethash system-class sys-table))
          (result order))
      (cond
        ((not (system-enabled-p system-class))
         (dolist (dependency (dependencies-of system))
           (setf result (enable-system dependency sys-table result)))
         (when (system-enabled-p system)
           (error (format nil "Circular dependency found for '~a'" system-class)))
         (log:debug "Enabling ~a" system-class)
         (enable system)
         (cons system-class result))
        (t result)))))


(defun enable-requested-systems (sys-table)
  (loop for system-class being the hash-key in sys-table
     with order = '() do
       (setf order (enable-system system-class sys-table order))
     finally (return order)))


(defun property (key &optional (default-value nil))
  (with-slots (properties) *engine*
    (%get-property key properties default-value)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *unloaded-foreign-libraries* nil)

  (defun unload-foreign-libraries ()
    (handler-bind ((style-warning #'muffle-warning))
      (loop for lib in (cffi:list-foreign-libraries) do
           (pushnew (cffi:foreign-library-name lib) *unloaded-foreign-libraries*)
           (cffi:close-foreign-library lib))))


  (defun reload-foreign-libraries ()
    (pushnew (merge-working-pathname (property :library-directory "lib/"))
             cffi:*foreign-library-directories*)
    (loop for lib-name in *unloaded-foreign-libraries* do
         (cffi:load-foreign-library lib-name)))


  #+sbcl
  (pushnew #'unload-foreign-libraries sb-ext:*save-hooks*))


(defun startup (properties-pathspec)
  (when *engine*
    (error "Engine already working"))
  (setf *engine* (make-instance 'bodge-engine))
  (in-new-thread-waiting "startup-worker"
    (with-slots (systems properties disabling-order shared-pool shared-executors
                         working-directory engine-lock)
        *engine*
      (setf properties (%load-properties properties-pathspec)
            shared-pool (make-pooled-executor
                         (property :engine-shared-pool-size 2))
            engine-lock (bt:make-recursive-lock "engine-lock")
            working-directory (uiop:pathname-directory-pathname properties-pathspec)
            shared-executors (list (make-single-threaded-executor)))

      (log:config (property :log-level :info))
      (reload-foreign-libraries)

      (let ((system-class-names
             (property :systems (lambda ()
                                  (error ":systems property should be defined")))))
        (setf systems (alist-hash-table (instantiate-systems system-class-names))
              disabling-order (enable-requested-systems systems))))))


(defun shutdown ()
  (unless *engine*
    (error "Engine already stopped"))
  (in-new-thread-waiting "shutdown-worker"
    (with-slots (systems disabling-order shared-pool shared-executors) *engine*
      (loop for system-class in disabling-order do
           (log:debug "Disabling ~a" system-class)
           (disable (gethash system-class systems)))
      (dispose shared-pool)
      (dolist (ex shared-executors)
        (dispose ex))))
  (setf *engine* nil))


(defun acquire-executor (&rest args &key (single-threaded-p nil) (exclusive-p nil)
                                      (special-variables nil))
  (with-slots (shared-pool shared-executors) *engine*
    (cond
      ((and (not exclusive-p) (not single-threaded-p) (not special-variables))
       shared-pool)
      ((and exclusive-p single-threaded-p)
       (make-single-threaded-executor special-variables))
      ((and single-threaded-p (not exclusive-p) (not special-variables))
       (first shared-executors))
      (t (error "Cannot provide executor for combination of requirements: ~a" args)))))


(defun shared-executor-p (executor shared-executors)
  (some (lambda (shared) (eq shared executor)) shared-executors))


(defun release-executor (executor)
  (with-slots (shared-pool shared-executors) *engine*
    (unless (or (shared-executor-p executor shared-executors)
                (eq executor shared-pool))
      (dispose executor))))


(defclass dispatcher () ())


(defmethod dispatch ((this bodge-engine) (task function) &rest keys &key (priority :medium)
                                                                      invariant same-thread-p)
  (with-slots (shared-pool) this
    (etypecase invariant
      ((or null symbol) (if same-thread-p
                         (funcall task)
                         (execute shared-pool task :priority priority)))
      (dispatcher (apply #'dispatch invariant task keys)))
    t))


(defmacro instantly ((&rest lambda-list) &body body)
  `(-> (:generic :same-thread-p t) (,@lambda-list)
     ,@body))


(define-flow value-flow (value)
  (instantly () value))


(define-flow null-flow ()
  (value-flow nil))


(defun run (fn &optional result-callback)
  (cl-flow:run-flow (engine) fn result-callback))

;;
(defgeneric system-of (obj))

(defclass system-object ()
  (system))


(defmethod system-of ((this system-object))
  (with-slots (system) this
    (tg:weak-pointer-value system)))


(defmethod initialize-instance :after ((this system-object) &key system)
  (with-slots ((this-system system)) this
    (setf this-system (tg:make-weak-pointer (ensure-not-null (or system *system*))))))


;;;
;;;
;;;
(defclass enableable ()
  ((enabled-p :initform nil)))


(defmethod enabledp ((this enableable))
  (with-slots (enabled-p) this
    enabled-p))


(defmethod enable ((this enableable))
  (call-next-method)
  (with-slots (enabled-p) this
    (setf enabled-p t)))


(defmethod disable ((this enableable))
  (call-next-method)
  (with-slots (enabled-p) this
    (setf enabled-p nil)))


;;;
;;;
;;;
(defclass handle ()
  ((value :initarg :value :initform (error ":value initarg missing") :reader value-of)))


(defclass foreign-object (disposable system-object)
  ((handle :initarg :handle :initform (error "foreign object :handle must be supplied")
           :reader handle-of)))


(definline handle-value-of (foreign-object)
  (with-slots (handle) foreign-object
    (value-of handle)))


(defgeneric destroy-foreign-object (handle))


(declaim (special *handle-value*))


(defmacro defhandle (name &key (initform nil)
                            (closeform (error ":closeform must be supplied")))
  (with-gensyms (handle value)
    `(progn
       (defclass ,name (handle) ())

       (defmethod destroy-foreign-object ((,handle ,name))
         (let ((*handle-value* (value-of ,handle)))
           ,closeform))

       (definline ,(symbolicate 'make- name) (&optional ,value)
         (make-instance ',name :value (or ,value ,initform
                                          (error "value or :initform must be provided")))))))


(define-destructor foreign-object ((handle handle-of) (sys system-of))
  (run
   (-> (sys :priority :low :important-p t) ()
     (destroy-foreign-object handle))))


(defmacro define-system-function (name system-class lambda-list &body body)
  (multiple-value-bind (forms decls doc) (parse-body body :documentation t)
    `(defun ,name ,lambda-list
       ,@(when doc (list doc))
       ,@decls
       (when-debugging
         (cond
           ((or (not (boundp '*system*)) (null *system*))
            (error (concatenate 'string "~a executed in the wrong system thread:"
                                " *system* unbound or nil, but ~a required")
                   ',name ',system-class))
           ((not (subtypep (class-of *system*) ',system-class))
            (error "~a executed in the wrong system thread: required ~a, but got ~a"
                   ',name ',system-class (and (class-name (class-of *system*)))))))
       ,@forms)))
