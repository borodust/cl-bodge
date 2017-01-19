(in-package :cl-bodge.poiu)


(define-constant +default-cleanup-size+ 10)


(defclass text-cache ()
  ((font :initarg :font :reader text-cache-font)
   (max-size :initarg :max-size)
   (cleanup-size :initform +default-cleanup-size+)
   (rated-queue :initform (make-rated-queue))
   (table :initform (make-hash-table :test #'equal) :reader text-cache-table)))


(defmethod initialize-instance :after ((this text-cache) &key)
  (with-slots (cleanup-size max-size) this
    (setf cleanup-size (min max-size +default-cleanup-size+))))


(defun make-text-cache (font &optional (max-size 1024))
  (make-instance 'text-cache :font font :max-size max-size))


(defun cleanup (text-cache)
  (with-slots (rated-queue table cleanup-size) text-cache
    (let ((evicted (loop for i from 0 below cleanup-size
                      collecting (let ((node (poprf rated-queue)))
                                   (remhash (string-of (rated-value node)) table)
                                   node))))
      (run (concurrently ()
             (loop for d in evicted
                do (dispose (rated-value d))))))))


(defun cache-text (text-cache string)
  (with-slots (rated-queue max-size font table cleanup-threshold) text-cache
    (let ((count (hash-table-count table))
          (text (make-text string font)))
      (when (= count max-size)
        (cleanup text-cache))
      (let ((node (pushrf text rated-queue)))
        (setf (gethash string table) node)))))


(defun promote-text (text-cache node)
  (with-slots (rated-queue) text-cache
    (promotef node rated-queue)
    node))


(define-system-function get-text graphics-system (text-cache string)
  (rated-value
   (if-let ((node (gethash string (text-cache-table text-cache))))
     (promote-text text-cache node)
     (cache-text text-cache string))))
