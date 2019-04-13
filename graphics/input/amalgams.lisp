(cl:in-package :cl-bodge.graphics)


(defvar *amalgam-descriptor-registry* (make-hash-table))


(defun register-amalgam-descriptor (class descriptor)
  (setf (gethash class *amalgam-descriptor-registry*) descriptor))


(defun find-amalgam-descriptor (class)
  (gethash class *amalgam-descriptor-registry*))


(defstruct amalgam-field
  (name nil :type symbol :read-only t)
  (type nil :type (or glsl-type glsl-opaque-type) :read-only t)
  (count 1 :type fixnum :read-only t)
  (foreign-name "" :type string :read-only t))


(defun make-amalgam-descriptor (fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name type &key name (count 1) &allow-other-keys) field
                  (let ((foreign-name (or name (string-downcase
                                                (ge.util:translate-name-to-foreign symbol-name)))))
                    (make-amalgam-field :name symbol-name
                                              :type (make-instance (type->glsl type))
                                              :count count
                                              :foreign-name foreign-name)))))


(defun amalgam-descriptor-fields (descriptor)
  descriptor)


(defclass amalgam () ())


(defun fields-to-descriptor (fields)
  (let ((expanded-field-descriptors (loop for field in fields
                                          collect (destructuring-bind (name type &rest opts)
                                                      (ensure-list field)
                                                    `(list ',name ',type ,@opts)))))
    `(make-amalgam-descriptor (list ,@expanded-field-descriptors))))


(defun fields-to-parameters (fields)
  (loop for field in fields
        collect (first (ensure-list field))))


(defun fields-to-slots (class-name fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name type &rest opts) (ensure-list field)
                  (declare (ignore type opts))
                  (let ((keyword-name (make-keyword symbol-name)))
                    `(,symbol-name :initform (error ,(with-output-to-string (s)
                                                       (prin1 keyword-name s)
                                                       (format s " missing")))
                                   :initarg ,keyword-name
                                   :accessor ,(symbolicate class-name '- symbol-name))))))


(defmacro defamalgam (name-and-opts &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    `(progn
       (defclass ,name (amalgam)
         (,@(fields-to-slots name fields)))
       (register-amalgam-descriptor ',name ,(fields-to-descriptor fields)))))


(defun full-field-name (field)
  (let* ((field-type (amalgam-field-type field))
         (field-count (amalgam-field-count field))
         (foreign-name (amalgam-field-foreign-name field))
         (full-foreign-name (cond
                              ((<= field-count 0)
                               (format nil "~A[]" foreign-name))
                              ((> field-count 1)
                               (format nil "~A[~A]" foreign-name field-count))
                              (t foreign-name))))
    (format nil "~A ~A;" (%glsl-name-of field-type) full-foreign-name)))


(defun print-amalgam-as-interface (amalgam-type interface-type &optional block-name (output t))
  (let ((descriptor (find-amalgam-descriptor amalgam-type)))
    (format output "~&layout(std140) ~A ~A_i {
~{~&  ~A~}
}"
            interface-type
            (translate-name-to-foreign amalgam-type)
            (loop for field in (amalgam-descriptor-fields descriptor)
                  collect (full-field-name field)))
    (when block-name
      (format output " ~A" block-name))
    (format output ";")))


(defun print-amalgam-as-uniforms (amalgam-type &optional (output t))
  (let ((descriptor (find-amalgam-descriptor amalgam-type)))
    (loop for field in (amalgam-descriptor-fields descriptor)
          do (format output "~&uniform ~A" (full-field-name field)))))


(defun print-amalgam-layout (amalgam-class &optional (out t))
  (when-let (descriptor (find-amalgam-descriptor amalgam-class))
    (format out "~A" descriptor)))
