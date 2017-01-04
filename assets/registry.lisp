(in-package :cl-bodge.assets)


(defgeneric asset-names (loader))
(defgeneric load-asset (loader name))
(defgeneric release-asset (loader name))
(defgeneric path-to (asset))


(defclass asset-registry (lockable)
  ((asset-table :initform (make-hash-table :test 'equal))))


(defun register-asset-loader (registry loader)
  (with-instance-lock-held (registry)
    (with-slots (asset-table) registry
      (loop for name in (asset-names loader)
         do (with-hash-entries ((asset-entry name)) asset-table
              (let* ((entry asset-entry))
                (when (and entry (not (eq entry loader)))
                  (warn "Asset redefinition: name ~A from ~A already registered with ~A loader"
                        name loader entry)))
              (setf asset-entry loader))))))


(defun release-assets (registry names)
  (with-instance-lock-held (registry)
    (with-slots (asset-table) registry
      (loop for name in names
         do (release-asset (gethash name asset-table) name)))))


(defun get-asset (registry name)
  (with-slots (asset-table) registry
    (if-let ((loader (gethash name asset-table)))
      (load-asset loader name)
      (null-flow))))
