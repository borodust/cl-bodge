(in-package :cl-bodge.resources)


(defvar *resource-storage* (make-instance 'resource-storage))


(defun mount-resource-provider (path provider)
  (let ((node-name (if (fad:directory-pathname-p path)
                       (enough-namestring path (fad:pathname-parent-directory path))
                       (file-namestring path))))
    (mount-storage-resource-node *resource-storage* path
                                 (funcall provider node-name))))


(defun mount-filesystem (resource-path filesystem-path)
  (mount-resource-provider resource-path
                           (make-filesystem-resource-provider filesystem-path)))


(defun engine-resource-name (name-control-string &rest args)
  (with-output-to-string (name-stream)
    (format name-stream  "/_engine/")
    (apply #'format name-stream name-control-string args)))


;;;
;;; Global resource registry
;;;
(defclass resource-registry (lockable)
  ((resource-table :initform (make-hash-table :test 'equal))))

(defvar *resource-registry* (make-instance 'resource-registry))


(defun register-resource (name handler)
  (with-instance-lock-held (*resource-registry*)
    (with-slots (resource-table) *resource-registry*
      (with-hash-entries ((resource-entry (namestring name))) resource-table
        (let ((entry resource-entry))
          (when (and entry (not (eq entry handler)))
            (warn "Resource redefinition: handler ~A for '~A' was registered earlier"
                  handler name)))
        (setf resource-entry handler)))))


(defun load-resource (name)
  (with-slots (resource-table) *resource-registry*
    (log:trace "Resource requested: '~A'" name)
    (when-let ((handler (gethash (namestring name) resource-table)))
      (with-resource-stream (stream name *resource-storage*)
        (decode-resource handler stream)))))


(defun resource-flow (&rest resource-names)
  (>> (~> (loop for name in resource-names
             collecting (multiple-value-bind (rsc flow-p)
                            (load-resource name)
                          (if flow-p
                              rsc
                              (value-flow rsc)))))))


(defun list-registered-resource-names ()
  (with-instance-lock-held (*resource-registry*)
    (with-slots (resource-table) *resource-registry*
      (loop for key being the hash-key of resource-table
         collect key))))


(defun find-resource-handler (resource-name)
  (with-slots (resource-table) *resource-registry*
    (with-instance-lock-held (*resource-registry*)
      (gethash (namestring resource-name) resource-table))))

;;;
;;; Define resource
;;;
(defmacro defresource (type resource-path &body opts &key path &allow-other-keys)
  (once-only (resource-path)
    `(progn
       (register-resource ,resource-path (make-resource-handler ,type ,@opts))
       ,@(when path
           `((mount-filesystem ,resource-path ,path))))))
