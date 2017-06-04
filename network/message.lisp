(in-package :cl-bodge.network)


(defgeneric message-id (message)
  (:method (message) (declare (ignore message) nil)))


(defgeneric reply-id (message)
  (:method (message) (declare (ignore message) nil)))


(defclass message () ())


(defgeneric make-message (message-class &key &allow-other-keys))
(defgeneric field-value (message field-name))


(defgeneric encode-message (message stream)
  (:method ((message message) stream)
    (conspack:encode message :stream stream)))


(defgeneric decode-message (stream)
  (:method (stream)
    (conspack:decode-stream stream)))


(defmacro defmessage (message-and-opts superclass &body slots)
  (destructuring-bind (message-name &rest options) (ensure-list message-and-opts)
    (let* ((superclasses (or (ensure-list superclass) (list 'message))))
      (assert (null (cdr superclasses)) (superclasses) "Multiple inheritance is not allowed")
      (flet ((process-slot-def (slot-def)
               (list slot-def :initform nil :initarg (make-keyword slot-def)
                     :reader (symbolicate (or (first (assoc-value options :reader-prefix))
                                              (symbolicate message-name '-))
                                          slot-def))))
        `(progn
           (defclass ,message-name ,superclasses
             (,@(mapcar #'process-slot-def slots))
             (:default-initargs ,@(assoc-value options :init)))
           (defmethod make-message ((class (eql ',message-name)) &rest initargs
                                    &key &allow-other-keys)
             (apply #'make-instance class initargs))
           (defmethod conspack:decode-object ((class (eql ',message-name)) alist &key)
             (apply #'make-message ',message-name (alist-plist alist)))
           ,@(loop for slot in slots
                collect `(defmethod field-value ((this ,message-name)
                                                 (field-name (eql ,(make-keyword slot))))
                           (slot-value this ',slot))))))))


(defmacro with-message-fields ((&rest fields) message &body body)
  (once-only (message)
    `(symbol-macrolet (,@(loop for field in fields
                            collect (destructuring-bind (name &optional value) (ensure-list field)
                                      (let ((value (or value name)))
                                        `(,name (field-value ,message ,(make-keyword value)))))))
       ,@body)))


(defmethod conspack:encode-object ((this message) &key)
  (loop for slot in (closer-mop:class-slots (class-of this))
     for slot-name = (closer-mop:slot-definition-name slot)
     collecting (cons (make-keyword slot-name) (slot-value this slot-name))))


(defmessage (identified-message (:reader-prefix message-)) ()
  id)


(defmessage reply-message (identified-message))


(defmethod reply-id ((this reply-message))
  (message-id this))


(defmessage (error-message (:reader-prefix error-)) (reply-message)
  text)


(defmessage ack-message (reply-message))


(defun make-reply-for (message reply-class &rest initargs &key &allow-other-keys)
  (apply #'make-message reply-class :id (message-id message) initargs))
