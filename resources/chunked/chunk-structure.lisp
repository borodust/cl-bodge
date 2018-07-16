(cl:in-package :cl-bodge.resources)


(declaim (special *objects*
                  *resolvers*))


(defun push-object (id obj)
  (setf (gethash id *objects*) obj))


(defun %map-value (val mapper)
  (if (listp (first val))
      (mapcar mapper val)
      (funcall mapper val)))


(defmacro define-chunk-structure (name-and-opts &body slots)
  (with-gensyms (obj)
    (destructuring-bind (name &optional treep child-type) (ensure-list name-and-opts)
      (let* ((ctor-name (symbolicate '%make- name))
             (slot-names (loop for slot in slots collecting
                              (if (listp slot)
                                  (first slot)
                                  slot)))
             (required-slots (if treep
                                 '(id children)
                                 '(id)))
             (all-slot-names (append required-slots slot-names)))
        (flet ((make-initarg (name)
                 `(,(make-keyword name) ,name))
               (expand-slot (slot-name)
                 `(,slot-name :initarg ,(make-keyword slot-name)
                              :accessor ,(symbolicate name '- slot-name)))
               (expand-required-slot (slot-name)
                 `(,slot-name :initarg ,(make-keyword slot-name)
                              :reader ,(symbolicate slot-name '-of)))
               (%ensure-list (arg-list)
                 `(let ((args ,(if treep
                                   `(first ,arg-list)
                                   `,arg-list)))
                    (ensure-list args)))
               (redefine-slot (slot id)
                 (destructuring-bind (slot-name type) slot
                   `(,slot-name
                     ,(if (eq :reference type)
                          `(%push-resolver #'(setf ,(symbolicate name '- slot-name))
                                           ,id
                                           ,slot-name)
                          `(%map-value ,slot-name #',(symbolicate 'make- type)))))))
          `(progn
             (defclass ,name ()
               (,@(mapcar #'expand-required-slot required-slots)
                ,@(mapcar #'expand-slot slot-names)))

             (defun ,(symbolicate 'make- name) (arg-list)
               (flet ((,ctor-name (,@all-slot-names)
                        (make-instance ',name
                                       ,@(reduce #'append
                                                 (mapcar #'make-initarg all-slot-names)))))
                 (destructuring-bind (id &key ,@slot-names) ,(%ensure-list 'arg-list)
                   (with-hash-entries ((,obj id)) *objects*
                     (unless (null ,obj)
                       (warn "Redefining object '~a'" id))
                     (let ,(loop for slot in slots
                              when (listp slot) collecting
                                (redefine-slot slot 'id))
                       (setf ,obj
                             ,(if treep
                                  `(,ctor-name
                                    id ,(if (null child-type)
                                            `(rest arg-list)
                                            `(mapcar #',(symbolicate 'make- child-type)
                                                     (rest arg-list)))
                                    ,@slot-names)
                                  `(,ctor-name id ,@slot-names))))))))))))))


(defgeneric serialize-chunk-structure (chunk))
(defgeneric parse-chunk-structure (type data))

;;;
;;; Structured resource
;;;
(defclass chunk-structure-resource-handler ()
  ((type :initarg :chunk-type :initform (error ":chunk-type missing") :reader chunk-type-of)))


(defmethod encode-resource ((this chunk-structure-resource-handler) resource stream)
  (with-standard-io-syntax
    (let ((*print-pretty* nil))
      (prin1 (serialize-chunk-structure resource) stream))))


(defmethod decode-resource ((this chunk-structure-resource-handler) stream)
  (with-slots (type) this
    (let ((stream (flex:make-flexi-stream stream :external-format :utf-8))
          (*objects* (make-hash-table :test 'equal))
          (*resolvers* (list)))
      (with-standard-io-syntax
        (let ((*read-eval* nil))
          (prog1 (parse-chunk-structure type (read-preserving-whitespace stream nil nil nil))
            (dolist (fn *resolvers*)
              (funcall fn))))))))
