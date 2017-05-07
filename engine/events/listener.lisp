(in-package :cl-bodge.events)


(defclass handler-table (disposable)
  ((table :initform (tg:make-weak-hash-table :weakness :key) :reader table-of)))


(defun %for-each-handler (table action)
  (loop for emitter being the hash-key of table using (hash-value handlers)
     do (loop for (event-class-name . handler) in handlers
           do (funcall action event-class-name emitter handler))))


(define-destructor handler-table (table)
  (%for-each-handler table #'unsubscribe-from))


;;
(defclass event-listening ()
  ((lock :initform (bt:make-recursive-lock "event-listener-lock"))
   (enabled-p :initform nil)
   (handler-table :initform (make-instance 'handler-table))))


(defun register-event-handler (event-listener event-class-name emitter handler)
  (with-slots (handler-table lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (subscribe-to event-class-name emitter handler))
      (pushnew (cons event-class-name handler)
               (gethash emitter (table-of handler-table))
               :test #'equal))))


(defun deregister-event-handler (event-listener event-class-name emitter handler)
  (with-slots (handler-table lock enabled-p) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (unsubscribe-from event-class-name emitter handler))
      (deletef (gethash emitter (table-of handler-table))
               (cons event-class-name handler)
               :test #'equal))))


(defun deregister-by-event-emitter (event-listener emitter)
  (with-slots (handler-table lock enabled-p) event-listener
    (when-let ((handlers (gethash emitter (table-of handler-table))))
      (loop for (event-class-name . handler) in handlers
         do (unsubscribe-from event-class-name emitter handler))
      (remhash emitter handler-table))))


(defun subscribe-listener (event-listener)
  (with-slots (lock enabled-p handler-table) event-listener
    (bt:with-recursive-lock-held (lock)
      (when enabled-p
        (error "Listener already subscribed"))
      (%for-each-handler (table-of handler-table) #'subscribe-to)
      (setf enabled-p t))))


(defun unsubscribe-listener (event-listener)
  (with-slots (lock enabled-p handler-table) event-listener
    (bt:with-recursive-lock-held (lock)
      (unless enabled-p
        (error "Listener already unsubscribed"))
      (%for-each-handler (table-of handler-table) #'unsubscribe-from)
      (setf enabled-p nil))))
