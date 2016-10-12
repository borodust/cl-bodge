(in-package :cl-bodge.audio)


(defclass al-object (disposable thread-bound-object)
  ((id :initarg :id :initform (error "id must be provided") :reader id-of)))
