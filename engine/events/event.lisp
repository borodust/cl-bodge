(in-package :cl-bodge.events)

;;;
;;;
;;;
(defclass event () ())


(defmacro defevent (name (&rest superclass-names) (&rest field-names) &rest class-options)
  (let ((constructor-name (symbolicate 'make- name)))
    `(progn
       (defclass ,name (,@superclass-names)
         (,@(loop for field-name in field-names collecting
                 `(,field-name :initarg ,(make-keyword field-name)
                               :initform (error "~a must be provided" ',field-name)
                               :reader ,(symbolicate field-name '-from))))
         ,@class-options)
       (declaim (inline ,constructor-name))
       (defun ,constructor-name (,@field-names)
         (make-instance ',name ,@(loop for field-name in field-names appending
                                      `(,(make-keyword field-name) ,field-name)))))))
