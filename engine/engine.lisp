(in-package :cl-bodge.engine)

;;
(defclass bodge-engine ()
  ((systems :initform nil)
   (properties :initform '())
   (disabling-order :initform '())))
(defvar *engine* (make-instance 'bodge-engine))


(defun engine-system (system-name)
  (with-slots (systems) *engine*
    (if-let ((system (gethash system-name systems)))
      system
      (error (format nil "~a not found" system-name)))))
      
;;
(defclass system ()
  ((dependencies :initarg :dependencies :initform '() :reader dependencies-of)))


(defgeneric enable (system))

(defgeneric disable (system))

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
  (with-slots (systems properties disabling-order) *engine*
    (setf properties (%load-properties properties-pathspec))
    (let ((system-class-names (property :systems
                                        (lambda ()
                                          (error ":systems property should be defined")))))
      (setf systems (alist-hash-table (instantiate-systems system-class-names))
            disabling-order (enable-requested-systems systems)))))


(defun shutdown ()
  (with-slots (systems disabling-order) *engine*
    (loop for system-class in disabling-order do
         (log:debug "Disabling ~a" system-class)
         (disable (gethash system-class systems)))))
