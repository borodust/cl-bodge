(cl:in-package :cl-bodge.utils)


(defclass array-slice ()
  ((dimensions :initform nil)
   (slice :initform nil)))


(defmethod initialize-instance :after ((this array-slice) &key dimensions)
  (with-slots ((dims dimensions) slice) this
    (setf slice (mapcar (constantly 0) dimensions)
          dims (reverse dimensions))))


(defun %next-slice (dimensions slice)
  (when slice
    (if (= (1- (first dimensions)) (first slice))
        (when (%next-slice (rest dimensions) (rest slice))
          (setf (first slice) 0))
        (incf (first slice)))))


(defun next-slice (obj)
  (with-slots (dimensions slice) obj
    (when (%next-slice dimensions slice)
      (reverse slice))))


(defun slice (obj)
  (with-slots (slice) obj
    slice))


(defun flat-index (obj)
  (with-slots (dimensions slice) obj
    (labels ((total-offset (offsets sizes prev)
               (if offsets
                   (+ (* (first offsets) (apply #'* prev))
                      (total-offset (rest offsets) (rest sizes) (cons (first sizes) prev)))
                   0)))
      (total-offset slice dimensions nil))))


(defun flatten-array (array)
  (let* ((dimensions (array-dimensions array))
         (slice (make-instance 'array-slice :dimensions dimensions))
         (result (make-array (apply #'* dimensions) :element-type (array-element-type array))))
    (loop for slice-indexes = (slice slice) then (next-slice slice) while slice-indexes
       do (setf (aref result (flat-index slice))
                (apply #'aref array slice-indexes)))
    result))


(defun expand-array (array dimensions)
  (unless (= (apply #'* (ensure-list dimensions)) (length array))
    (error "incorrect dimensions"))
  (let* ((slice (make-instance 'array-slice :dimensions dimensions))
         (result (make-array dimensions :element-type (array-element-type array))))
    (loop for slice-indexes = (slice slice) then (next-slice slice) while slice-indexes
       do (setf (apply #'aref result slice-indexes)
                (aref array (flat-index slice))))
    result))


(defun find-dimensions (sequence)
  (typecase sequence
    (array (array-dimensions sequence))
    (list (cons (length sequence) (find-dimensions (elt sequence 0))))))


(defmacro float-array (initial-contents)
  (once-only (initial-contents)
    `(make-array (find-dimensions ,initial-contents)
                 :element-type 'single-float
                 :initial-contents ,initial-contents)))


(defmacro with-simple-array-pointer ((pointer-var simple-array) &body body)
  (once-only (simple-array)
    #+sbcl
    `(sb-sys:with-pinned-objects (,simple-array)
       (let ((,pointer-var (sb-sys:vector-sap (sb-ext:array-storage-vector ,simple-array))))
         ,@body))
    #+ccl
    `(ccl:with-pointer-to-ivector (,pointer-var ,simple-array)
       ,@body)
    #+ecl
    `(let ((,pointer-var (si:make-foreign-data-from-array ,simple-array)))
       ,@body)
    #-(or sbcl ccl ecl)
    (error "with-simple-array-pointer is not implemented for ~A" (lisp-implementation-type))))
