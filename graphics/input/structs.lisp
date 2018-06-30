(cl:in-package :cl-bodge.graphics)


(defvar *struct-descriptor-registry* (make-hash-table))


(defun register-shader-struct-descriptor (class descriptor)
  (setf (gethash class *struct-descriptor-registry*) descriptor))


(defun find-shader-struct-descriptor (class)
  (gethash class *struct-descriptor-registry*))


(defstruct shader-struct-field
  (name nil :type symbol :read-only t)
  (foreign-name "" :type string :read-only t))


(defun make-shader-struct-descriptor (fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name &key name &allow-other-keys) field
                  (let ((foreign-name (or name (ge.util:translate-name-to-foreign symbol-name))))
                    (make-shader-struct-field :name symbol-name
                                              :foreign-name foreign-name)))))


(defun shader-struct-descriptor-fields (descriptor)
  descriptor)


(defclass shader-structure () ())


(defun fields-to-descriptor (fields)
  (let ((expanded-field-descriptors (loop for field in fields
                                          collect (destructuring-bind (name &rest opts)
                                                      (ensure-list field)
                                                    `(list ',name ,@opts)))))
    `(make-shader-struct-descriptor (list ,@expanded-field-descriptors))))


(defun fields-to-parameters (fields)
  (loop for field in fields
        collect (first (ensure-list field))))


(defun fields-to-slots (class-name fields)
  (loop for field in fields
        collect (destructuring-bind (symbol-name &rest opts) (ensure-list field)
                  (declare (ignore opts))
                  (let ((keyword-name (make-keyword symbol-name)))
                    `(,symbol-name :initform (error ,(with-output-to-string (s)
                                                       (prin1 keyword-name s)
                                                       (format s " missing")))
                                   :initarg ,keyword-name
                                   :accessor ,(symbolicate class-name '- symbol-name))))))


(defmacro defsstruct (name-and-opts &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (declare (ignore opts))
    (with-gensyms (initargs)
      `(progn
         (defclass ,name (shader-structure)
           (,@(fields-to-slots name fields)))
         (defun ,(symbolicate 'make- name) (&rest ,initargs
                                            &key ,@(fields-to-parameters fields) &allow-other-keys)
           (declare (ignore ,@(fields-to-parameters fields)))
           (apply #'make-instance ',name ,initargs))
         (register-shader-struct-descriptor ',name ,(fields-to-descriptor fields))))))


(defmethod inject-shader-input ((this shader-structure) &key name)
  (let ((fields (shader-struct-descriptor-fields
                 (find-shader-struct-descriptor (class-name-of this)))))
    (loop for field in fields
          for field-name = (shader-struct-field-name field)
          for foreign-name = (shader-struct-field-foreign-name field)
          for value = (slot-value this field-name)
          do (inject-shader-input value :name (format nil "~A.~A" name foreign-name)))))
