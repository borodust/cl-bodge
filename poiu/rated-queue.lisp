(in-package :cl-bodge.poiu)


(define-constant +rating-threshold+ 0.95d0 :test #'=)


(defun current-timestamp ()
  (floor (* (real-time-seconds) 1000)))


(defclass rated-queue-node ()
  ((created :initform (current-timestamp) :type fixnum
            :reader rated-node-created)
   (last-accessed :initform 0 :type fixnum
                  :accessor rated-node-last-accessed)
   (value :initarg :value
          :reader rated-node-value)
   (next :initform nil :initarg :next :type (or rated-queue-node null)
         :accessor rated-node-next)
   (prev :initform nil :type (or rated-queue-node null)
         :accessor rated-node-prev)))


(defmethod initialize-instance :after ((this rated-queue-node) &key)
  (with-slots (last-accessed created) this
    (setf last-accessed created)))


(defun make-rated-queue-node (value &optional next-node)
  (make-instance 'rated-queue-node :value value :next next-node))


(defun node-rating (node current-timestamp)
  (let* ((last-accessed (rated-node-last-accessed node))
         (created (rated-node-created node))
         (alive (- current-timestamp created)))
    (if (= 0 alive)
        0d0
        (float (/ (- last-accessed created) alive) 0d0))))


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
  (rated-node-value element))


(defun return-promoted (head node prev next)
  (if (or (eq prev node) (eq next node))
      head
      (let ((node-next (rated-node-next node)))
        (exclude-node node)
        (insert-node node prev next)
        (if (eq head node)
            node-next
            head))))


(defun %promote (head node timestamp)
  (loop with prev = node and current-rating = (node-rating node timestamp)
     for next = (rated-node-next node) then (rated-node-next next)
     for next-rating = (if (null next) 1d0 (node-rating next timestamp))
     until (or (>= next-rating current-rating)
               (>= next-rating +rating-threshold+))
     do (setf prev next)
     finally (return (return-promoted head node prev next))))


(defun push-rated (value queue)
  (insert-node (make-rated-queue-node value) nil queue))


(defun pop-rated (queue)
  (unless (null queue)
    (values (rated-node-next queue) (exclude-node queue))))


(defmacro pushrf (value queue)
  `(setf ,queue (push-rated ,value ,queue)))


(defmacro poprf (queue)
  (with-gensyms (q n)
    `(multiple-value-bind (,q ,n) (pop-rated ,queue)
       (setf ,queue ,q)
       ,n)))


(defun %promotef (node queue)
  (let ((timestamp (current-timestamp)))
    (setf (rated-node-last-accessed node) timestamp)
    (%promote queue node timestamp)))


(defmacro promotef (node queue)
  `(setf ,queue (%promotef ,node ,queue)))
