(in-package :cl-bodge.graphics)


(declaim (special *active-buffer*))


(defhandle buffer-handle
    :initform (gl:gen-buffer)
    :closeform (gl:delete-buffers (list *handle-value*)))

;;
;;
(defclass buffer (gl-object)
  ((target :initarg :target :initform (error "Buffer target should be provided")
           :reader target-of))
  (:default-initargs :handle (make-buffer-handle)))


(defgeneric attach-array-buffer (buffer target index))


(defmacro with-bound-buffer ((buffer) &body body)
  (once-only (buffer)
    `(unwind-protect
          (progn
            (gl:bind-buffer (target-of ,buffer) (handle-value-of ,buffer))
            (let ((*active-buffer* ,buffer))
              ,@body))
       (if-bound *active-buffer*
                 (gl:bind-buffer (target-of *active-buffer*) (handle-value-of *active-buffer*))
                 (gl:bind-buffer (target-of ,buffer) 0)))))

;;
;;
(defclass array-buffer (buffer)
  ((attribute-size :initform 0 :reader attribute-size-of)
   (vertex-count :initform 0 :reader vertex-count-of))
  (:default-initargs :target :array-buffer))


(define-system-function make-array-buffer graphics-system
    (vertex-attribute-data &key (system *system*))
  (make-instance 'array-buffer
                 :system system
                 :vertex-attribute-data vertex-attribute-data))


(defmethod initialize-instance :after ((this array-buffer) &key vertex-attribute-data)
  (declare (type (or (simple-array * (*))
                     (simple-array * (* 1))
                     (simple-array * (* 2))
                     (simple-array * (* 3))
                     (simple-array * (* 4)))
                 vertex-attribute-data))
  (destructuring-bind (data-vertex-count &optional (data-attrib-size 1 attrib-size-provided-p))
      (array-dimensions vertex-attribute-data)
    (flet ((component-type ()
             (let ((c (if attrib-size-provided-p
                          (aref vertex-attribute-data 0 0)
                          (aref vertex-attribute-data 0 ))))
               (etypecase c
                 (integer :int)
                 (single-float :float)))))
      (with-slots (attribute-size vertex-count) this
        (setf attribute-size data-attrib-size
              vertex-count data-vertex-count)
        (gl:with-gl-array (gl-array (component-type) :count (* vertex-count attribute-size))
          (map-to-gl-array vertex-attribute-data gl-array)
          (with-bound-buffer (this)
            (gl:buffer-data :array-buffer :static-draw gl-array)))))))


;;;
;;;
;;;
(defclass index-buffer (buffer)
  ((index-count :initform nil :reader index-count-of))
  (:default-initargs :target :element-array-buffer))


(defmethod initialize-instance :after ((this index-buffer) &key index-array)
  (with-slots (index-count) this
    (setf index-count (length index-array))
    (gl:with-gl-array (gl-array :uint :count index-count)
      (map-to-gl-array index-array gl-array)
      (with-bound-buffer (this)
        (gl:buffer-data :element-array-buffer :static-draw gl-array)))))


(define-system-function make-index-buffer graphics-system (index-array &key (system *system*))
  (make-instance 'index-buffer :system system :index-array index-array))
