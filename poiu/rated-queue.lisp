(in-package :cl-bodge.poiu)


(defclass rated-queue-node ()
  ((timestamp :initarg :timestamp :type fixnum :reader rated-node-timestamp)
   (request-count :initform 1 :type integer :accessor rated-node-request-count)
   (value :initarg :value :reader rated-node-value)
   (next :initform nil :initarg :next :type (or rated-queue-node null) :accessor rated-node-next)
   (prev :initform nil :type (or rated-queue-node null) :accessor rated-node-prev)))


(defun current-timestamp ()
  (floor (* (real-time-seconds) 10)))


(defun make-rated-queue-node (value &optional next-node)
  (make-instance 'rated-queue-node :timestamp (current-timestamp) :value value :next next-node))


(defun node-rating (node current-timestamp)
  (let ((elapsed (- current-timestamp (rated-node-timestamp node))))
    (if (> elapsed 0)
        (/ (rated-node-request-count node) elapsed)
        0)))


(defun exclude-node (node)
  (when-let ((prev (rated-node-prev node)))
    (setf (rated-node-next prev) (rated-node-next node)))
  (when-let ((next (rated-node-next node)))
    (setf (rated-node-prev next) (rated-node-prev node)))
  node)


(defun insert-node (node prev next)
  (when prev
    (setf (rated-node-next prev) node))
  (when next
    (setf (rated-node-prev next) node))
  (setf (rated-node-next node) next
        (rated-node-prev node) prev)
  node)


(defun make-rated-queue ()
  nil)


(defun rated-value (element)
  (rated-node-value (car element)))


(defun push-rated (value queue)
  (insert-node (make-rated-queue-node value) nil queue))


(defun pop-rated (queue)
  (unless (null queue)
    (values (rated-node-next queue) (exclude-node queue))))


(defun %promote (head node timestamp)
  (loop with prev = node and new-head = head
     for next = (rated-node-next node) then (rated-node-next next)
     until (or (null next) (> (node-rating next timestamp)
                              (node-rating node timestamp)))
     do (setf prev next)
     finally
       (unless (eq node prev)
         (when (null (rated-node-prev node))
           (setf new-head (rated-node-next node)))
         (exclude-node node)
         (insert-node node prev next))
       (return new-head)))


(defun %demote (head node timestamp)
  (loop with next = node and new-head = head
     for prev = (rated-node-prev node) then (rated-node-prev prev)
     until (or (null prev) (< (node-rating prev timestamp)
                              (node-rating node timestamp)))
     do (setf next prev)
     finally
       (unless (eq node next)
         (when (null prev)
           (setf new-head node))
         (exclude-node node)
         (insert-node node prev next))
       (return new-head)))


(defun promote (node head)
  (let ((timestamp (current-timestamp)))
    (incf (rated-node-request-count node))
    (let ((head (%promote head node timestamp)))
      (%demote head node timestamp))))


(defmacro pushrf (value queue)
  `(setf ,queue (push-rated ,value ,queue)))


(defmacro poprf (queue)
  (with-gensyms (q n)
    `(multiple-value-bind (,q ,n) (pop-rated ,queue)
       (setf ,queue ,q)
       ,n)))


(defmacro promotef (node queue)
  `(setf ,queue (promote ,queue ,node)))
