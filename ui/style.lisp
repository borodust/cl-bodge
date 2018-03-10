(cl:in-package :cl-bodge.ui)

(declaim (special *style*))


(defclass style ()
  ((style-table :initarg :style-table :initform (make-hash-table :test 'equal))))


(defun style (&rest path)
  (with-slots (style-table) *style*
    (gethash path style-table)))


(defun (setf style) (value &rest path)
  (with-slots (style-table) *style*
    (setf (gethash path style-table) value)))


(defun make-style ()
  (make-instance 'style))


(defun make-default-style ()
  (let ((style (make-hash-table-with-entries (:test 'equal)
                 ('(:row-height) 26)
                 ('(:layout-spacing) 4))))
    (make-instance 'style :style-table style)))
