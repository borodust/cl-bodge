(in-package :cl-bodge.engine)

(defvar *engine-startup-hooks* nil)
(defvar *system-startup-hooks* (mt:make-guarded-reference (make-hash-table)))
(defvar *system-shutdown-hooks* (mt:make-guarded-reference (make-hash-table)))

(declaim (special *system*))

;;
(defclass bodge-engine (lockable)
  ((systems :initform nil)
   (properties :initform '())
   (working-directory :initform nil :reader working-directory-of)
   (shared-executors :initform nil)
   (shared-pool :initform nil)
   (disabling-order :initform '())))

(defvar *engine* nil)


(definline engine ()
  "Return engine instance"
  *engine*)


(defun engine-system (system-name)
  "Return engine's system instance by class name. Throws error if system cannot be found."
  (with-slots (systems) (engine)
    (with-instance-lock-held ((engine))
      (if-let ((system (gethash system-name systems)))
        system
        (error (format nil "~a not found" system-name))))))


(defun after-system-startup (system-name hook)
  (mt:with-guarded-reference (hooks *system-startup-hooks*)
    (push hook (gethash system-name hooks))))


(defun invoke-system-startup-hooks (system-name)
  (mt:with-guarded-reference (hooks *system-startup-hooks*)
    (log-errors
      (dolist (hook (gethash system-name hooks))
        (funcall hook)))))


(defun before-system-shutdown (system-name hook)
  (mt:with-guarded-reference (hooks *system-shutdown-hooks*)
    (push hook (gethash system-name hooks))))


(defun invoke-system-shutdown-hooks (system-name)
  (mt:with-guarded-reference (hooks *system-shutdown-hooks*)
    (log-errors
      (dolist (hook (gethash system-name hooks))
        (funcall hook)))))


(defun working-directory ()
  "Return working directory of the engine which corresponds to the directory where configuration
file is stored."
  (working-directory-of (engine)))


(defun merge-working-pathname (pathname)
  "Merge `pathname` with engine's working directory."
  (merge-pathnames pathname (working-directory)))


;;
(defclass system ()
  ((dependencies :initarg :depends-on :initform '() :reader dependencies-of))
  (:documentation "Base class for all engine systems"))


(defgeneric enable (system)
  (:method (system) nil)
  (:documentation "Enable engine's system synchronousy."))


(defgeneric disable (system)
  (:method (system) nil)
  (:documentation "Disable engine's system synchronously."))


(defgeneric enabledp (system)
  (:documentation "Check if `system` is enabled. Must be thread-safe."))


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
         (invoke-system-startup-hooks system-class)
         (cons system-class result))
        (t result)))))


(defun enable-requested-systems (sys-table)
  (loop for system-class being the hash-key in sys-table
     with order = '() do
       (setf order (enable-system system-class sys-table order))
     finally (return order)))


(defun property (key &optional (default-value nil))
  "Return engine's property provided in the configuration file or `default-value` if it was not
specified."
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


(defun startup (properties &optional (working-directory (truename (uiop:getcwd))))
  "Start engine synchronously loading configuration from `properties` (file, hash-table, plist
or alist). `(:engine :systems)` property must be present in the config and should contain list
of existing system class names loaded into current lisp image. Specified systems and their
dependencies will be initialized in the correct order according to a dependency graph. All
directories used by the engine are relative to 'working-directory parameter."
  (when *engine*
    (error "Engine already running"))
  (setf *engine* (make-instance 'bodge-engine))
  (log:config :sane2)
  (in-new-thread-waiting "startup-worker"
    (with-slots (systems (this-properties properties)
                         (this-working-directory working-directory)
                         disabling-order shared-pool shared-executors)
        *engine*
      (setf this-properties (%load-properties properties)
            shared-pool (make-pooled-executor
                         (property :engine-shared-pool-size 2))
            this-working-directory working-directory
            shared-executors (list (make-single-threaded-executor)))
      (log:config (property '(:engine :log-level) :info))
      (reload-foreign-libraries)
      (log-errors
        (dolist (hook *engine-startup-hooks*)
          (funcall hook)))
      (let ((system-class-names (property '(:engine :systems))))
        (when (null system-class-names)
          (error "(:engine :systems) property should be defined and cannot be nil"))
        (setf systems (alist-hash-table (instantiate-systems system-class-names))
              disabling-order (enable-requested-systems systems))))))


(defun shutdown ()
  "Shutdown engine synchronously and deinitialize systems in the reverse order they were
initialized."
  (unless *engine*
    (error "Engine already stopped"))
  (in-new-thread-waiting "shutdown-worker"
    (with-slots (systems disabling-order shared-pool shared-executors) *engine*
      (loop for system-class in disabling-order
         do
           (log:debug "Disabling ~a" system-class)
           (invoke-system-shutdown-hooks system-class)
           (disable (gethash system-class systems)))
      (dispose shared-pool)
      (dolist (ex shared-executors)
        (dispose ex))))
  (setf *engine* nil))


(defun acquire-executor (&rest args &key (single-threaded-p nil) (exclusive-p nil)
                                      (special-variables nil))
  "Acquire executor from the engine, properties of which correspond to provided options:
:single-threaded-p - if t, executor will be single-threaded, otherwise it can be pooled one
:exclusive-p - if t, this executor cannot be acquired by other requester and :special-variables
               can be specified for it, otherwise this executor could be shared among different
               requesters."
  (with-slots (shared-pool shared-executors) *engine*
    (cond
      ((and (not exclusive-p) (not single-threaded-p) (not special-variables))
       shared-pool)
      ((and exclusive-p single-threaded-p)
       (make-single-threaded-executor :special-variables special-variables))
      ((and single-threaded-p (not exclusive-p) (not special-variables))
       (first shared-executors))
      (t (error "Cannot provide executor for combination of requirements: ~a" args)))))


(defun shared-executor-p (executor shared-executors)
  (some (lambda (shared) (eq shared executor)) shared-executors))


(defun release-executor (executor)
  "Release executor back to the engine"
  (with-slots (shared-pool shared-executors) *engine*
    (unless (or (shared-executor-p executor shared-executors)
                (eq executor shared-pool))
      (dispose executor))))


(defclass dispatcher () ()
  (:documentation "Base class for all engine dispatchers"))


(defmethod dispatch ((this bodge-engine) (task function) invariant &rest keys
                     &key (priority :medium) concurrently)
  "Use engine instance as a dispatcher. If :invariant and :concurrently-p are both nil task is
not dispatched and just executed in the current thread. If :invariant is nil but :concurrently-p
is t, task is dispatched to the engine's default pooled executor. If :invariant is specified,
task is dispatched to the object provided under this key."
  (with-slots (shared-pool) this
    (etypecase invariant
      (null (if concurrently
                (execute shared-pool task :priority priority)
                (funcall task)))
      (dispatcher (apply #'dispatch invariant task nil keys)))
    t))


(defmacro instantly ((&rest lambda-list) &body body)
  "Execute task in the current thread without dispatching."
  `(-> nil ,lambda-list
     ,@body))


(defmacro concurrently ((&rest lambda-list) &body body)
  "Push task to engine's pooled executor."
  `(-> (nil :concurrently t) ,lambda-list
     ,@body))


(defun value-flow (value)
  "Return flow that returns single value."
  (instantly () value))


(defun null-flow ()
  "Return flow that returns nil as single value."
  (value-flow nil))


(defgeneric initialization-flow (object &key &allow-other-keys)
  (:documentation "Return flow that initializes an object.
Flow variant of #'initialize-instance, although no guarantees
about object returned from the flow are provided.")
  (:method (object &key &allow-other-keys)))


(defun assembly-flow (class &rest initargs &key &allow-other-keys)
  "Return flow that constructs an object and returns it.
Flow variant of #'make-instance."
  (let ((instance (apply #'make-instance class :allow-other-keys t initargs)))
    (>> (apply #'initialization-flow instance initargs)
        (value-flow instance))))


(flet ((dispatch-dynamically (task invariant &rest opts &key &allow-other-keys)
         (apply #'dispatch (engine) task invariant opts)))
  (defun run (flow)
    "Dispatch flow using engine as a dispatcher."
    (cl-flow:run #'dispatch-dynamically flow)))

;;
(defgeneric system-of (obj)
  (:documentation "Return system obj has relation with."))


(defclass system-object ()
  (system)
  (:documentation "Base class for objects that has any relationship with particular system."))


(defmethod system-of ((this system-object))
  "Return system this object has relationship with."
  (with-slots (system) this
    (tg:weak-pointer-value system)))


(defmethod initialize-instance :after ((this system-object) &key system)
  (with-slots ((this-system system)) this
    (setf this-system (tg:make-weak-pointer (ensure-not-null (or system *system*))))))


;;;
;;;
;;;
(defclass enableable ()
  ((enabled-p :initform nil))
  (:documentation "Mixin for systems for making them enableable: conform to
 enabledp/enable/disable interface."))


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
(declaim (special *handle-value*))


(defclass handle ()
  ((value :initarg :value :initform (error ":value initarg missing") :reader value-of)))


(defgeneric destroy-handle (handle))
(defgeneric handle-of (object))


(definline handle-value-of (object)
  "Return value stored in the handle of the provided foreign object"
  (value-of (handle-of object)))


(defmacro defhandle (name &key (initform nil)
                            (closeform (error ":closeform must be supplied")))
  "Define foreign object handle that keeps track of foreign instance. :closeform has access to
*handle-value* which contains foreign instance that must be disposed. Foreign instance can be
initialized and returned by :initform or provided to the generated handle constructor ('make- +
`name`)."
  (with-gensyms (handle value)
    `(progn
       (defclass ,name (handle) ())

       (defmethod destroy-handle ((,handle ,name))
         (let ((*handle-value* (value-of ,handle)))
           ,closeform))

       (definline ,(symbolicate 'make- name) (&optional ,value)
         (make-instance ',name :value (or ,value ,initform
                                          (error "value or :initform must be provided")))))))


(defclass foreign-object (disposable)
  ((handle :initarg :handle :initform (error "foreign object :handle must be supplied")
           :reader handle-of))
  (:documentation "Base class for disposable foreign objects. Simplifies
handling of init/dispose lifecycle for such ojects."))


(define-destructor foreign-object ((handle handle-of))
  (destroy-handle handle))


(defclass system-foreign-object (disposable system-object)
  ((handle :initarg :handle :initform (error "foreign object :handle must be supplied")
           :reader handle-of))
  (:documentation "Base class for disposable system-dependent foreign objects. Simplifies
handling of init/dispose lifecycle for such ojects."))


(define-destructor system-foreign-object ((handle handle-of) (sys system-of))
  (run (-> (sys :priority :low :important-p t) ()
         (destroy-handle handle))))


;;;
;;;
;;;
(defmacro define-system-function (name system-class lambda-list &body body)
  "Define function that bound to the system, i.e. its body must be executed only when *system*
special variable corresponds to the instance of the `system-class`."
  (multiple-value-bind (forms decls doc) (parse-body body :documentation t)
    `(defun ,name ,lambda-list
       ,@(when doc (list doc))
       ,@decls
       (in-development-mode
         (cond
           ((or (not (boundp '*system*)) (null *system*))
            (error (concatenate 'string "~a executed in the wrong system thread:"
                                " *system* unbound or nil, but ~a required")
                   ',name ',system-class))
           ((not (subtypep (class-of *system*) ',system-class))
            (error "~a executed in the wrong system thread: required ~a, but got ~a"
                   ',name ',system-class (and (class-name (class-of *system*)))))))
       ,@forms)))
