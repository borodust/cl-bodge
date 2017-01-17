(in-package :cl-bodge.poiu)


(defclass text-cache ()
  ((font :initarg :font :reader text-cache-font)
   (max-size :initarg :max-size)
   (table :initform (make-hash-table :test #'equal) :reader text-cache-table)))


(defun make-text-cache (font &optional (max-size 1024))
  (make-instance 'text-cache :font font :max-size max-size))


(definline get-text (text-cache string)
  (if-let ((text (gethash string (text-cache-table text-cache))))
    text
    (setf (gethash string (text-cache-table text-cache))
          (make-text string (text-cache-font text-cache)))))
