(in-package :cl-bodge.assets)


(declaim (special *objects*
                  *resource-path*
                  *resolvers*))


(defgeneric parse-chunk (chunk-type parameters data)
  (:method (chunk-type parameters data)
    (error "Unknown chunk type: ~a" chunk-type)))


(defgeneric read-chunk (chunk-type parameters stream)
  (:method (chunk-type parameters stream)
    (parse-chunk chunk-type parameters
                 (read-preserving-whitespace
                  (flexi-streams:make-flexi-stream stream :external-format :utf-8)))))


(defun push-object (id obj)
  (setf (gethash id *objects*) obj))


(defun merge-relative (relative-path)
  (fad:merge-pathnames-as-file *resource-path* relative-path))


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


(defclass resource ()
  ((chunks :initarg :chunks)))


(defun find-chunk (resource chunk-name)
  (with-slots (chunks) resource
    (gethash chunk-name chunks)))


(defun list-chunks (resource)
  (with-slots (chunks) resource
    (loop for chunk being the hash-value of chunks using (hash-key name)
         collect (cons name chunk))))


(defun read-deflated (in chunk-type parameters compression)
  (if compression
      (ecase compression
        (:deflate
         (destructuring-bind (&key (compressed-size (error ":compressed-size missing"))
                                   &allow-other-keys)
             parameters
           (let* ((start-position (file-position in))
                  (inflated (chipz:make-decompressing-stream 'chipz:deflate in)))
             (prog1 (read-chunk chunk-type parameters inflated)
               ;; fixme: dirty hack? mb better to allocate buffer from source stream
               (file-position in (+ start-position compressed-size)))))))
      (read-chunk chunk-type parameters in)))


(defun load-asset (path)
  (flet ((resolve-references (resolvers)
           (dolist (fn resolvers)
             (funcall fn))))
    (with-open-file (in (fad:canonical-pathname path) :element-type '(unsigned-byte 8))
      (let ((char-stream (flexi-streams:make-flexi-stream in :external-format :utf-8)))
        (destructuring-bind (format version) (read char-stream)
          (unless (eq :brf format)
            (error "Unknown format: ~a" format))
          (unless (eql 1 version)
            (error "Unsupported version: ~a" version))
          (let* ((*objects* (make-hash-table :test 'equal))
                 (*resource-path* path)
                 (*resolvers* '())
                 (chunk-table (make-hash-table :test 'equal)))
            (loop for chunk-header = (read-preserving-whitespace char-stream nil nil nil)
               until (null chunk-header) do
                 (destructuring-bind (chunk-type &rest parameters &key name compression
                                                 &allow-other-keys)
                     chunk-header
                   (with-hash-entries ((chunk name)) chunk-table
                     (cond
                       ((null name)
                        (error "Nameless chunk header of type ~A found" chunk-type))
                       ((not (null chunk))
                        (error "Duplicate chunk with name ~A found" name)))
                     (setf chunk (read-deflated in chunk-type parameters compression)))))
            (resolve-references *resolvers*)
            (make-instance 'resource :chunks chunk-table)))))))


;;;
;;;
;;;

(defstruct cached-resource
  chunk
  value)


(defclass resource-loader (lockable dispatching)
  ((chunk-table :initform (make-hash-table :test 'equal))))


(defmethod initialize-instance :after ((this resource-loader) &key resource-paths)
  (with-slots (chunk-table) this
    (loop for path in resource-paths
       for chunks = (list-chunks (load-asset path))
       do (loop for (name . chunk) in chunks
             do (with-hash-entries ((cached name)) chunk-table
                  (when cached
                    (warn "Redefining resource with name ~A to ~A" name path))
                  (setf cached (make-cached-resource :chunk chunk)))))))


(defun make-resource-loader (&rest resource-paths)
  (make-instance 'resource-loader :resource-paths resource-paths))


(defmethod list-resource-names ((this resource-loader))
  (with-slots (chunk-table) this
    (loop for name being the hash-key of chunk-table
       collect name)))


(defgeneric chunk-asset-flow (chunk loader))
(defgeneric dispose-chunk-asset (chunk asset loader)
  (:method (chunk asset loader)))


(defmethod load-resource ((this resource-loader) name)
  (with-slots (chunk-table) this
    (if-let ((cached (gethash name chunk-table)))
      (with-instance-lock-held (this)
        (if-let ((cached-value (cached-resource-value cached)))
          (value-flow cached-value)
          (let ((chunk (cached-resource-chunk cached)))
            (>> (chunk-asset-flow chunk this)
                (instantly (asset)
                  (setf (cached-resource-value cached) asset))))))
      (null-flow))))


(defmethod release-asset ((this resource-loader) name)
  (with-slots (chunk-table) this
    (with-instance-lock-held (this)
      (when-let* ((cached (gethash name chunk-table))
                  (cached-value (cached-resource-value cached)))
        (dispose-chunk-asset (cached-resource-chunk cached) cached-value this)
        (setf (cached-resource-value cached) nil)))))
