(in-package :cl-bodge.event)


(defclass event-system (generic-system)
  ((enabled-p :initform nil)
   (handler-table :initform (make-hash-table))
   (thread-pool :initform (make-thread-pool 4))))


(defmethod enabledp ((this event-system))
  (slot-value this 'enabled-p))


(defmethod initialize-system ((this event-system))
  (with-slots (enabled-p thread-pool) this
    (open-pool thread-pool)
    (setf enabled-p t)))


(defmethod discard-system ((this event-system))
  (with-slots (enabled-p thread-pool handler-table) this
    (close-pool thread-pool)
    (clrhash handler-table)
    (setf enabled-p nil)))


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


(declaim (ftype (function (event event-system) *) post))
(defun post (event event-system)
  (with-slots (thread-pool handler-table) event-system
    (with-system-lock-held (event-system)
      (%check-event-class-registration (class-of event) event-system))
    (within-pool (thread-pool)
      (with-system-lock-held (event-system)
        (loop for handler in (gethash (class-of event) handler-table) do
             (within-pool (thread-pool)
               (funcall handler event)))))))


(declaim (ftype (function (symbol (function (event) *) event-system) *) subscribe-to))
(defun subscribe-to (event-class-name handler event-system)
  (let ((event-class (find-class event-class-name)))
    (with-slots (thread-pool handler-table) event-system
      (with-system-lock-held (event-system)
        (%check-event-class-registration event-class event-system))
      (within-pool (thread-pool)
        (with-system-lock-held (event-system)
          (with-hash-entries ((handlers event-class)) handler-table
            (pushnew handler handlers)))))))


(defmacro subscribe-with-handler-body-to (event-class event-system
                                          (&optional (event-var (gensym)))
                                          &body body)
  `(subscribe-to ',event-class (lambda (,event-var)
                                 (declare (ignorable ,event-var)) ,@body) ,event-system))
