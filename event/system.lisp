(in-package :cl-bodge.event)


(defclass event-system (system)
  ((enabled-p :initform nil)
   (handler-table :initform (make-hash-table))
   (lock :initform (make-recursive-lock "event-sys-lock"))
   (thread-pool :initform (make-thread-pool 4))))


(defmethod enable ((this event-system))
  (with-slots (enabled-p lock thread-pool) this
    (with-lock-held (lock)
      (when enabled-p
        (error "Event system already running"))
      (open-pool thread-pool)
      (setf enabled-p t))))


(defmethod disable ((this event-system))
  (with-slots (enabled-p lock thread-pool) this
    (with-lock-held (lock)
      (unless enabled-p
        (error "Event system already stopped"))
      (close-pool thread-pool)
      (setf enabled-p nil))))


;;;
;;;
;;;
(defclass event () ())


(defun event-class-registered-p (event-class event-system)
  (with-slots (lock handler-table) event-system
    (with-recursive-lock-held (lock)
      (multiple-value-bind (handler-list present-p) (gethash event-class handler-table)
        (declare (ignore handler-list))
        present-p))))


(defun register-event-class (event-class event-system)
  (with-slots (lock handler-table) event-system
    (with-recursive-lock-held (lock)
      (if (event-class-registered-p event-class event-system)
          (error "Event class ~a already registered" event-class)
          (setf (gethash event-class handler-table) '())))))


(defun register-event-classes (event-system &rest event-class-names)
  (loop for event-class-name in event-class-names do
       (register-event-class (find-class event-class-name) event-system)))


(defun %check-event-class-registration (event-class event-system)
  (with-slots (handler-table) event-system
    (unless (event-class-registered-p event-class event-system)
      (error "Unrecognized event class ~a" event-class))))


(declaim (ftype (function (event event-system) *) post))
(defun post (event event-system)
  (with-slots (thread-pool lock handler-table) event-system
    (with-recursive-lock-held (lock)
      (%check-event-class-registration (class-of event) event-system))
    (within-pool (thread-pool)
      (with-recursive-lock-held (lock)
        (loop for handler in (gethash (class-of event) handler-table) do
             (within-pool (thread-pool)
               (funcall handler event)))))))
      


(declaim (ftype (function (symbol (function (event) *) event-system) *) subscribe-to))
(defun subscribe-to (event-class-name handler event-system)
  (let ((event-class (find-class event-class-name)))
    (with-slots (thread-pool lock handler-table) event-system
      (with-recursive-lock-held (lock)
        (%check-event-class-registration event-class event-system))
      (within-pool (thread-pool)
        (with-recursive-lock-held (lock)
          (with-hash-entries ((handlers event-class)) handler-table
            (pushnew handler handlers)))))))
