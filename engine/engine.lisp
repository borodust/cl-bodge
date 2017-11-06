(in-package :cl-bodge.engine)

(defvar *engine* nil)

(defvar *engine-startup-hooks* nil)
(defvar *system-startup-hooks* (mt:make-guarded-reference (make-hash-table)))
(defvar *system-shutdown-hooks* (mt:make-guarded-reference (make-hash-table)))
(defvar *predefined-event-callbacks* nil)
(defvar *executable-p* nil)

(declaim (special *system*))

(define-constant +engine-resource-path+ "/bodge/"
  :test #'equal)

;;
(defclass bodge-engine (disposable lockable)
  ((systems :initform nil)
   (event-emitter :initform nil :reader event-emitter-of)
   (properties :initform '())
   (working-directory :initform nil :reader working-directory-of)
   (shared-executors :initform nil)
   (shared-pool :initform nil)
   (disabling-order :initform '())))


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


(defun executablep ()
  *executable-p*)

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
    (bodge-blobs-support:close-foreign-libraries)
    (handler-bind ((style-warning #'muffle-warning))
      (loop for lib in (cffi:list-foreign-libraries)
         when (cffi:foreign-library-loaded-p lib)
         do (progn
              (pushnew (cffi:foreign-library-name lib) *unloaded-foreign-libraries*)
              (cffi:close-foreign-library lib)))))


  (defun reload-foreign-libraries (library-directory)
    (bodge-blobs-support:register-library-directory (merge-working-pathname library-directory))
    (bodge-blobs-support:load-foreign-libraries)
    (loop for lib-name in *unloaded-foreign-libraries*
       do (cffi:load-foreign-library lib-name)))


  (defun mark-executable ()
    (setf *executable-p* t))


  #+sbcl
  (ge.util:unionf sb-ext:*save-hooks* (list #'unload-foreign-libraries
                                            #'mark-executable)))


(defun enable-systems (engine)
  (with-slots (systems disabling-order) engine
    (let ((system-class-names (property '(:engine :systems))))
      (when (null system-class-names)
        (error "(:engine :systems) property should be defined and cannot be nil"))
      (setf systems (alist-hash-table (instantiate-systems system-class-names))
            disabling-order (enable-requested-systems systems)))))


(defun disable-systems (engine)
  (with-slots (systems disabling-order) engine
    (loop for system-class in disabling-order do
         (log:debug "Disabling ~a" system-class)
         (invoke-system-shutdown-hooks system-class)
         (disable (gethash system-class systems)))))


(defmethod initialize-instance :after ((this bodge-engine)
                                       &key properties working-directory)
  (with-slots (systems (this-properties properties)
                       (this-working-directory working-directory)
                       shared-pool shared-executors event-emitter)
      this
    (setf this-properties (%load-properties properties)
          shared-pool (make-pooled-executor
                       (%get-property :engine-shared-pool-size this-properties 2))
          this-working-directory working-directory
          shared-executors (list (make-single-threaded-executor))
          event-emitter (make-instance 'event-emitting))
    (loop for (event-class-name . handlers) in *predefined-event-callbacks*
       do (dolist (handler handlers)
            (subscribe-to event-class-name event-emitter handler)))))


(define-destructor bodge-engine (shared-pool shared-executors)
  (dispose shared-pool)
  (dolist (ex shared-executors)
    (dispose ex)))


(defun startup (properties &optional (working-directory (directory-namestring
                                                         (current-executable-path))))
  "Start engine synchronously loading configuration from `properties` (file, hash-table, plist
or alist). `(:engine :systems)` property must exist in the config and should contain list
of existing system class names loaded into current lisp image. Specified systems and their
dependencies will be initialized in the correct order according to a dependency graph. All
directories used by the engine are relative to 'working-directory parameter."
  (when *engine*
    (error "Engine already running"))
  (setf *engine* (make-instance 'bodge-engine
                                :properties properties
                                :working-directory working-directory))
  (log:config :sane2)
  (log:config (property '(:engine :log-level) :info))
  (reload-foreign-libraries (property '(:engine :library-directory) "lib/"))
  (log-errors
    (dolist (hook *engine-startup-hooks*)
      (funcall hook)))
  (in-new-thread-waiting "startup-worker"
    (enable-systems *engine*))
  (values))


(defun shutdown ()
  "Shutdown engine synchronously and deinitialize systems in the reverse order they were
initialized."
  (unless *engine*
    (error "Engine already stopped"))
  (in-new-thread-waiting "shutdown-worker"
    (disable-systems *engine*))
  (dispose *engine*)
  (setf *engine* nil)
  (values))


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


(defclass dispatching () ()
  (:documentation "Mixin class for all engine dispatchers"))


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
      (dispatching (apply #'dispatch invariant task nil keys)))
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


(defmethod initialization-flow :around (object &key &allow-other-keys)
  (>> (call-next-method)
      (instantly ()
        (initialize-destructor object))))


(defun assembly-flow (class &rest initargs &key &allow-other-keys)
  "Return flow that constructs an object and returns it.
Flow variant of #'make-instance."
  (let* ((*auto-initialize-destructor* nil)
         (instance (apply #'make-instance class :allow-other-keys t initargs)))
    (>> (apply #'initialization-flow instance initargs)
        (value-flow instance))))


(flet ((%dispatch (task invariant &rest opts &key &allow-other-keys)
         (apply #'dispatch (engine) task invariant opts)))
  (defun run (flow)
    "Dispatch flow using engine as a dispatcher."
    (cl-flow:run #'%dispatch flow)))

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


(defun register-predefined-callback (event-class-name fn-name)
    (pushnew fn-name (assoc-value *predefined-event-callbacks* event-class-name)))
