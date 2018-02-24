(cl:in-package :cl-bodge.graphics)


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
(defgeneric update-array-buffer (buffer data))


(definline use-buffer (buffer)
  (gl:bind-buffer (target-of buffer) (handle-value-of buffer)))


(defmacro with-bound-buffer ((buffer) &body body)
  (once-only (buffer)
    `(unwind-protect
          (progn
            (use-buffer ,buffer)
            (let ((*active-buffer* ,buffer))
              ,@body))
       (if-bound *active-buffer*
                 (use-buffer *active-buffer*)
                 (gl:bind-buffer (target-of ,buffer) 0)))))

;;
;;
(defclass array-buffer (buffer)
  ((attribute-size :initform 0 :reader attribute-size-of)
   (vertex-count :initform 0 :reader vertex-count-of))
  (:default-initargs :target :array-buffer))


(define-system-function make-array-buffer graphics-system (vertex-attribute-data)
  (make-instance 'array-buffer
                 :system *system*
                 :vertex-attribute-data vertex-attribute-data))


(defun %update-array-buffer (this array-data)
  (declare (type (or (simple-array * (*))
                     (simple-array * (* 1))
                     (simple-array * (* 2))
                     (simple-array * (* 3))
                     (simple-array * (* 4)))
                 array-data))
  (destructuring-bind (element-count &optional (element-size 1 attrib-size-provided-p))
      (array-dimensions array-data)
    (let ((component-type (let ((c (if attrib-size-provided-p
                                       (aref array-data 0 0)
                                       (aref array-data 0 ))))
                            (etypecase c
                              (integer :int)
                              (single-float :float)))))
      (with-slots (attribute-size vertex-count) this
        (setf attribute-size element-size
              vertex-count element-count)
        (gl:with-gl-array (gl-array component-type :count (* vertex-count attribute-size))
          (map-to-gl-array array-data gl-array)
          (with-bound-buffer (this)
            (%gl:buffer-data :array-buffer
                             (gl:gl-array-byte-size gl-array)
                             (cffi:null-pointer)
                             :static-draw)
            (gl:buffer-sub-data :array-buffer gl-array)))))))


(defmethod initialize-instance :after ((this array-buffer) &key vertex-attribute-data)
  (%update-array-buffer this vertex-attribute-data))


(defmethod update-array-buffer ((this array-buffer) array-data)
  (%update-array-buffer this array-data))


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


(define-system-function make-index-buffer graphics-system (index-array)
  (make-instance 'index-buffer :system *system* :index-array index-array))
