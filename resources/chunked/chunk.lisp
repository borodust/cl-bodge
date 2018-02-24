(cl:in-package :cl-bodge.resources)


(defun %map-value (val mapper)
  (if (listp (first val))
      (mapcar mapper val)
      (funcall mapper val)))


(defun %push-resolver (accessor cur-id ref-ids)
  (flet ((%get-object (ref-id)
           (if-let ((ref (gethash ref-id *objects*)))
             ref
             (log:warn "Cannot resolve reference: object '~a' undefined" ref-id))))
    (prog1 nil
      (push (lambda ()
              (when-let ((refs (remove-if #'null (mapcar #'%get-object (ensure-list ref-ids)))))
                (funcall accessor (if (listp ref-ids) refs (first refs))
                         (gethash cur-id *objects*))))
            *resolvers*))))


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


;;;
;;; Chunked resource
;;;
(defclass chunk-resource-handler ()
  ((type :initarg :chunk-type :initform (error ":chunk-type missing") :reader chunk-type-of)))


(defgeneric convert-from-chunk (handler chunk)
  (:method (handler chunk)
    (declare (ignore handler))
    chunk))


(defgeneric convert-to-chunk (handler resource)
  (:method (handler resource)
    (declare (ignore handler))
    resource))


(defmethod encode-resource ((this chunk-resource-handler) resource stream)
  (with-slots (type) this
    (write-chunk (convert-to-chunk this resource) stream)))


(defmethod decode-resource ((this chunk-resource-handler) stream)
  (with-slots (type) this
    (convert-from-chunk this (read-deflated stream type nil))))
