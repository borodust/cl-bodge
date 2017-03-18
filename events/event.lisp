(in-package :cl-bodge.event)


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


(defun event-class-registered-p (event-class event-system)
  (with-slots (handler-table) event-system
    (with-system-lock-held (event-system)
      (multiple-value-bind (handler-list present-p) (gethash event-class handler-table)
        (declare (ignore handler-list))
        present-p))))


(defun register-event-class (event-class-name event-system)
  (with-slots (handler-table) event-system
    (let ((event-class (find-class event-class-name)))
      (with-system-lock-held (event-system)
        (if (event-class-registered-p event-class event-system)
            (error "Event class ~a already registered" event-class)
            (setf (gethash event-class handler-table) '()))))))


(defun register-event-classes (event-system &rest event-class-names)
  (loop for event-class-name in event-class-names do
       (register-event-class event-class-name event-system)))


(defun %check-event-class-registration (event-class event-system)
  (with-slots (handler-table) event-system
    (unless (event-class-registered-p event-class event-system)
      (error "Unrecognized event class ~a" event-class))))


(defun post (event event-system)
  (with-slots (executor handler-table) event-system
    (with-system-lock-held (event-system)
      (%check-event-class-registration (class-of event) event-system)
      (execute executor
               (lambda ()
                 (loop for handler in (with-system-lock-held (event-system)
                                        (gethash (class-of event) handler-table))
                    do (etypecase handler
                         (function (funcall handler event))
                         (symbol (funcall (symbol-function handler) event)))))))))


(defun subscribe-to (event-class-name handler event-system)
  (let ((event-class (find-class event-class-name)))
    (with-slots (executor handler-table) event-system
      (with-system-lock-held (event-system)
        (%check-event-class-registration event-class event-system)
        (execute executor
                 (lambda ()
                   (with-system-lock-held (event-system)
                     (with-hash-entries ((handlers event-class)) handler-table
                       (pushnew handler handlers))))))))
  handler)
