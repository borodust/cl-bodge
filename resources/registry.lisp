(in-package :cl-bodge.resources)

;;;
;;;  Resource loader
;;;
(defgeneric list-resource-names (loader)
  (:method (loader) (declare (ignore loader)) nil))

(defgeneric load-resource (loader name))

(defgeneric release-resource (loader name)
  (:method (loader name) (declare (ignore loader name))))


(defclass resource-registry (lockable)
  ((resource-table :initform (make-hash-table :test 'equal))))

(defvar *resource-registry* (make-instance 'resource-registry))


(defun register-resource-loader (loader)
  (with-instance-lock-held (*resource-registry*)
    (with-slots (resource-table) *resource-registry*
      (loop for name in (list-resource-names loader)
         do (with-hash-entries ((resource-entry name)) resource-table
              (let* ((entry resource-entry))
                (when (and entry (not (eq entry loader)))
                  (warn "Resource redefinition: name ~A from ~A already registered with ~A loader"
                        name loader entry)))
              (setf resource-entry loader))))))


(defun release-resources (names)
  (with-instance-lock-held (*resource-registry*)
    (with-slots (resource-table) *resource-registry*
      (loop for name in names
         do (release-resource (gethash name resource-table) name)))))


(defun get-resource (name)
  (with-slots (resource-table) *resource-registry*
    (log:trace "Resource requested: '~A'" name)
    (when-let ((loader (gethash name resource-table)))
      (load-resource loader name))))


(defun resource-flow (&rest resource-names)
  (>> (~> (loop for name in resource-names
             collecting (multiple-value-bind (rsc value-p)
                            (get-resource name)
                          (if value-p
                              (value-flow rsc)
                              rsc))))
      (instantly (resources)
        (values-list (mapcar #'first resources)))))
