(cl:in-package :cl-bodge.resources)


(defparameter *scene-read-buffer-size* (* 64 1024))


(declaim (special *scene-read-buffer*))


(defun read-descriptor (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (block nil
        (handler-bind ((end-of-file (lambda (c) (declare (ignore c)) (return))))
          (read-preserving-whitespace stream nil nil nil))))))


(defun for-each-descriptor (stream action)
  (let ((stream (flex:make-flexi-stream stream :external-format :utf-8)))
    (loop for descriptor = (read-descriptor stream)
          while descriptor
          do (destructuring-bind (descriptor-type &rest args &key &allow-other-keys)
                 descriptor
               (apply action descriptor-type args)))))


(defmacro do-descriptors ((descriptor-lambda-list stream) &body body)
  (with-gensyms (args)
    `(for-each-descriptor ,stream (lambda (&rest ,args)
                                    (destructuring-bind (,@descriptor-lambda-list) ,args
                                      ,@body)))))


(defclass scene ()
  ((meshes :initform nil :accessor %meshes-of)))


(defclass mesh ()
  ((id :initarg :id :initform (error ":id missing"))
   (primitive :initarg :primitive :initform (error ":primitive missing"))
   (position-array :initform nil :accessor %position-array-of)
   (index-array :initform nil :accessor %index-array-of)
   (normal-array :initform nil :accessor %normal-array-of)))


(defun read-buffer (stream &key (count (error ":count missing"))
                             (element-size 1)
                             (element-type (error ":element-type missing"))
                    &allow-other-keys)
  (let* ((type (ecase element-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))))
         (byte-length (* count element-size (claw:sizeof element-type)))
         (array (make-array (* count element-size) :element-type type)))
    (with-simple-array-pointer (dst-ptr array)
      (loop with byte-offset = 0
            with dst-addr = (cffi:pointer-address dst-ptr)
            with src-ptr = (static-vectors:static-vector-pointer *scene-read-buffer*)
            for bytes-read = (read-sequence *scene-read-buffer* stream
                                            :end (min (- byte-length byte-offset)
                                                      *scene-read-buffer-size*))
            while (> bytes-read 0)
            do (claw:memcpy (cffi:make-pointer (+ dst-addr byte-offset))
                            src-ptr
                            :n bytes-read
                            :type :char)
               (incf byte-offset bytes-read)
            finally (unless (= byte-offset byte-length)
                      (error "Premature end of stream: expected ~A, got ~A"
                             byte-length byte-offset))))
    array))


(defun read-mesh (stream &key index primitive size)
  (let ((mesh (make-instance 'mesh :id index :primitive primitive)))
    (do-descriptors ((attribute-type &rest args) (make-bounded-input-stream stream size))
      (ecase attribute-type
        (:position-array (setf (%position-array-of mesh) (apply #'read-buffer stream args)))
        (:index-array (setf (%index-array-of mesh) (apply #'read-buffer stream args)))
        (:normal-array (setf (%normal-array-of mesh) (apply #'read-buffer stream args)))))
    mesh))


(defun read-scene (stream)
  (static-vectors:with-static-vector (*scene-read-buffer* *scene-read-buffer-size*)
    (let ((scene (make-instance 'scene)))
      (do-descriptors ((type &rest params) stream)
        (ecase type
          (:mesh (push (apply #'read-mesh stream params) (%meshes-of scene)))))
      scene)))

;;;
;;; Scene resource
;;;
(defclass scene-resource-handler () ())


(defmethod decode-resource ((this scene-resource-handler) stream)
  (read-scene stream))


(defmethod encode-resource ((this image-resource-handler) resource stream)
  (error "Unimplemented"))


(defmethod make-resource-handler ((type (eql :scene)) &key)
  (make-instance 'scene-resource-handler))
