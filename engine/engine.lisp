(in-package :cl-bodge.engine)

;;
(defclass bodge-engine ()
  ((systems :initform nil)
   (properties :initform '())
   (shared-executors :initform nil)
   (shared-pool :initform nil)
   (disabling-order :initform '())))

(defvar *engine* (make-instance 'bodge-engine))

(definline engine ()
  *engine*)

(defun engine-system (system-name)
  (with-slots (systems) *engine*
    (if-let ((system (gethash system-name systems)))
      system
      (error (format nil "~a not found" system-name)))))

;;
(defclass system ()
  ((dependencies :initarg :depends-on :initform '() :reader dependencies-of)))


(defgeneric enable (system))

(defgeneric disable (system))

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


(defun startup (properties-pathspec)
  (in-new-thread-waiting "startup-worker"
    (with-slots (systems properties disabling-order shared-pool shared-executors) *engine*
      (setf properties (%load-properties properties-pathspec)
            shared-pool (make-pooled-executor
                         (property :engine-shared-pool-size 2))
            shared-executors (list (make-single-threaded-executor)))
      (let ((system-class-names
             (property :systems (lambda ()
                                  (error ":systems property should be defined")))))
        (setf systems (alist-hash-table (instantiate-systems system-class-names))
              disabling-order (enable-requested-systems systems))))))


(defun shutdown ()
  (in-new-thread-waiting "shutdown-worker"
    (with-slots (systems disabling-order shared-pool shared-executors) *engine*
      (loop for system-class in disabling-order do
           (log:debug "Disabling ~a" system-class)
           (disable (gethash system-class systems)))
      (dispose shared-pool)
      (dolist (ex shared-executors)
        (dispose ex)))))


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


(defmethod dispatch ((this bodge-engine) (task function) &key priority)
  (with-slots (shared-pool) this
    (execute shared-pool task :priority priority)
    t))

;;
(defgeneric system-of (obj))

(defclass system-object ()
  (system))


(defmethod system-of ((this system-object))
  (with-slots (system) this
    (tg:weak-pointer-value system)))


(defmethod initialize-instance :after ((this system-object) &key system)
  (with-slots ((this-system system)) this
    (setf this-system (tg:make-weak-pointer (ensure-not-null system)))))


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
