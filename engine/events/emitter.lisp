(cl:in-package :cl-bodge.events)


;;
(defclass handler-registry (lockable)
  ((handler-table :initform (make-hash-table))))


(defun invoke-handlers (reg event)
  (with-slots (handler-table) reg
    (destructuring-bind (&optional handler-lock &rest handlers)
        (with-instance-lock-held (reg)
          (gethash (class-of event) handler-table))
      (when handler-lock
        (flet ((acquire-rest (list)
                 (bt:with-recursive-lock-held (handler-lock)
                   (rest list))))
          (loop for handler in handlers by #'acquire-rest
             do (etypecase handler
                  (function (funcall handler event))
                  (symbol (funcall (symbol-function handler) event)))))))))


(defun register-handler (reg event-class handler)
  (with-slots (handler-table) reg
    (with-instance-lock-held (reg)
      (with-hash-entries ((locked-handlers event-class)) handler-table
        (let ((handlers (or locked-handlers
                            (setf locked-handlers
                                  (list (bt:make-recursive-lock "handler-list-lock"))))))
          (push handler (cdr handlers)))))))


(defun remove-handler (reg event-class handler)
  (with-slots (handler-table) reg
    (with-instance-lock-held (reg)
      (when-let ((handler-list (gethash event-class handler-table)))
        (deletef (cdr handler-list) handler)))))


;;
(defclass event-emitting ()
  ((handler-registry :initform (make-instance 'handler-registry))))


(defun fire-event (event emitter)
  (with-slots (handler-registry) emitter
    (invoke-handlers handler-registry event)))


(defgeneric subscribe-to (event-class-name emitter handler)
  (:method (event-class-name (emitter event-emitting) handler)
    (with-slots (handler-registry) emitter
      (let ((event-class (find-class event-class-name)))
        (register-handler handler-registry event-class handler)))
    handler))


(defgeneric unsubscribe-from (event-class-name emitter handler)
  (:method (event-class-name (emitter event-emitting) handler)
    (with-slots (handler-registry) emitter
      (let ((event-class (find-class event-class-name)))
        (remove-handler handler-registry event-class handler)))
    handler))


(defmacro %with-accessor-bindings ((accessor-bindings accessor-package event-var) &body body)
  (let ((bindings (loop for binding in accessor-bindings
                     for (name accessor) = (if (and (listp binding) (second binding))
                                               binding
                                               (list binding (symbolicate binding '-from)))
                     collect `(,name (,(format-symbol accessor-package "~A" accessor) ,event-var)))))
    `(let ,bindings
       ,@body)))


(defmacro subscribe-body-to ((event-class emitter (&rest accessor-bindings)
                                          &optional (event-var (gensym)))
                             &body body)
  `(subscribe-to ',event-class ,emitter
                 (lambda (,event-var)
                   (declare (ignorable ,event-var))
                   (%with-accessor-bindings (,accessor-bindings
                                             ,(package-name (symbol-package event-class))
                                             ,event-var)
                     ,@body))))
