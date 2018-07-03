(cl:in-package :cl-bodge.memory)


(defgeneric foreign-pointer-of (object))


(defmethod foreign-pointer-of ((object null))
  (cffi:null-pointer))


(defclass foreign-array (disposable)
  ((array :initarg :array :type (simple-array * *) :reader simple-array-of)))


(define-destructor foreign-array (array)
  (free-static-vector array))


(defun make-foreign-array (length &key (element-type '(unsigned-byte 8))
                                    (initial-element nil initial-element-p)
                                    (initial-contents nil initial-contents-p))
  (make-instance 'foreign-array
                 :array (cond
                          (initial-element-p
                           (make-static-vector length
                                               :element-type element-type
                                               :initial-element initial-element))
                          (initial-contents-p
                           (make-static-vector length
                                               :element-type element-type
                                               :initial-contents initial-contents))
                          (t (make-static-vector length
                                                 :element-type element-type)))))


(defmethod foreign-pointer-of ((this foreign-array))
  (with-slots (array) this
    (static-vector-pointer array)))
