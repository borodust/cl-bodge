(cl:in-package :cl-bodge.events)


;;;
;;;
;;;
(defvar *event-slot-table* (make-hash-table))


(defclass event () ())



(defmacro defevent (name (&rest superclass-names) &body fields-and-options)
  (destructuring-bind ((&rest field-names) . class-options) fields-and-options
    (let ((constructor-name (symbolicate 'make- name))
          (all-field-names (loop for superclass-name in superclass-names
                                 append (gethash superclass-name *event-slot-table*) into superslots
                                 finally (return
                                           (append superslots (mapcar #'make-keyword field-names))))))
      (setf (gethash name *event-slot-table*) all-field-names)
      `(progn
         (defclass ,name ,(if superclass-names
                              superclass-names
                              '(event))
           (,@(loop for field-name in field-names collecting
                    `(,field-name :initarg ,(make-keyword field-name)
                                  :initform (error ":~a missing" ',field-name)
                                  :reader ,(symbolicate field-name '-from))))
           ,@class-options)
         (declaim (inline ,constructor-name))
         (defun ,constructor-name (,@(mapcar #'symbolicate all-field-names))
           (make-instance ',name ,@(loop for field-name in all-field-names appending
                                         `(,(make-keyword field-name) ,(symbolicate field-name)))))))))
