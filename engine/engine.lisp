(in-package :cl-bodge.engine)

;;
(defclass bodge-engine ()
  ((systems :initform nil)
   (properties :initform '())
   (thread-pool :initform nil)
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
  (with-slots (systems properties disabling-order thread-pool) *engine*
    (setf properties (%load-properties properties-pathspec)
          thread-pool (make-thread-pool (property :engine-thread-pool-size 4)))
    (open-pool thread-pool)
    (let ((system-class-names (property :systems
                                        (lambda ()
                                          (error ":systems property should be defined")))))
      (setf systems (alist-hash-table (instantiate-systems system-class-names))
            disabling-order (enable-requested-systems systems)))))


(defun shutdown ()
  (with-slots (systems disabling-order thread-pool) *engine*
    (loop for system-class in disabling-order do
         (log:debug "Disabling ~a" system-class)
         (disable (gethash system-class systems)))
    (close-pool thread-pool)))


(defmethod execute ((this bodge-engine) fn)
  (with-slots (thread-pool) this
    (with-promise (resolve reject)
      (push-to-pool thread-pool
                    (lambda ()
                      (handler-case
                          (resolve (funcall fn))
                        (t (e)
                          (log:error e)
                          (reject e))))))))


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
