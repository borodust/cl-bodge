(cl:in-package :cl-bodge.resources)


(declaim (special *objects*
                  *resolvers*))


(defun push-object (id obj)
  (setf (gethash id *objects*) obj))


(defun %map-value (val mapper)
  (if (listp (first val))
      (mapcar mapper val)
      (funcall mapper val)))


(defgeneric serialize-chunk-structure (chunk-object))
(defgeneric deserialize-chunk-structure (chunk-type arg-list))

(defmacro define-chunk-structure (name-and-opts &body slots)
  (with-gensyms (arg-list applied-ctor obj)
    (destructuring-bind (name) (ensure-list name-and-opts)
      (let* ((slot-names (loop for slot in slots
                               collecting (if (listp slot)
                                              (first slot)
                                              slot))))
        (labels ((expand-initarg (name &optional type subtype)
                   (unless type
                     (error "Malformed compound chunk structure record ~A" name))
                   `(,(make-keyword name)
                     ,(case type
                        (:list `(flet ((,applied-ctor (,arg-list)
                                         (apply #',(format-symbol (symbol-package subtype) "~A~A"
                                                                  'make- subtype)
                                                ,arg-list)))
                                  (mapcar #',applied-ctor ,name)))
                        (t (let ((ctor (format-symbol (symbol-package type) "~A~A" 'make- type)))
                             `(apply #',ctor ,name))))))
                 (make-initarg (slot)
                   (if (listp slot)
                       (apply #'expand-initarg slot)
                       `(,(make-keyword slot) ,slot)))
                 (make-simple-initarg (slot)
                   (let ((name (first (ensure-list slot))))
                     `(,(make-keyword name) ,name)))
                 (expand-slot (slot-name)
                   `(,slot-name :initarg ,(make-keyword slot-name)
                                :accessor ,(symbolicate name '- slot-name))))
          `(progn
             (defclass ,name ()
               (,@(mapcar #'expand-slot slot-names)))
             (defun ,(format-symbol (symbol-package name) "~A~A" 'make- name) (&key ,@slot-names)
               (make-instance ',name
                              ,@(reduce #'append
                                        (mapcar #'make-simple-initarg slots))))
             (defmethod deserialize-chunk-structure ((,name (eql ',name)) ,arg-list)
               (destructuring-bind (&key ,@slot-names) (ensure-list ,arg-list)
                 (make-instance ,name
                                ,@(reduce #'append
                                          (mapcar #'make-initarg slots)))))
             (defmethod serialize-chunk-structure ((,name ,name))
               (list ,@(loop for slot in slots
                             appending (if (listp slot)
                                           (destructuring-bind (slot-name type &optional subtype) slot
                                             (declare (ignore subtype))
                                             (list (make-keyword slot-name)
                                                   `(let ((,obj (slot-value ,name ',slot-name)))
                                                      ,(case type
                                                         (:list `(mapcar #'serialize-chunk-structure ,obj))
                                                         (t `(serialize-chunk-structure ,obj))))))
                                           (list (make-keyword slot)
                                                 `(slot-value ,name ',slot))))))))))))


;;;
;;; Structured resource
;;;
(defclass chunk-structure-resource-handler (resource-handler)
  ((type :initarg :chunk-type :initform (error ":chunk-type missing") :reader chunk-type-of)))


(defmethod encode-resource ((this chunk-structure-resource-handler) resource stream)
  (with-standard-io-syntax
    (let ((*print-pretty* nil))
      (let ((stream (flex:make-flexi-stream stream :external-format :utf-8)))
          (prin1 (serialize-chunk-structure resource) stream)))))


(defmethod decode-resource ((this chunk-structure-resource-handler) stream)
  (with-slots (type) this
    (let ((stream (flex:make-flexi-stream stream :external-format :utf-8)))
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (deserialize-chunk-structure type (read-preserving-whitespace stream nil nil nil)))))))
