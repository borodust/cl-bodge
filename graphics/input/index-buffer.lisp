(cl:in-package :cl-bodge.graphics)


(defgeneric index-buffer-length (array)
  (:method ((array null)) 0))


(defclass index-buffer-input (disposable shader-input)
  ((buffer-id)
   (length :reader index-buffer-length)))


(define-destructor index-buffer-input (buffer-id)
  (gl:delete-buffers (list buffer-id)))


(defmethod initialize-instance :after ((this index-buffer-input) &key data)
  (with-slots (buffer-id length) this
    (let ((id (gl:gen-buffer)))
      (bind-for-serious-condition ((lambda () (gl:delete-buffers (list id))))
        (gl:bind-buffer :element-array-buffer id)
        (let ((*buffer-target* :element-array-buffer))
          (fill-number-buffer data :unsigned-int))
        (setf buffer-id id
              length (length data))
        (gl:bind-buffer :element-array-buffer 0)))))


(defun make-index-buffer (data)
  (make-instance 'index-buffer-input :data data))


(defmethod inject-shader-input ((this index-buffer-input) &key)
  (with-slots (buffer-id) this
    (gl:bind-buffer :element-array-buffer buffer-id)))
