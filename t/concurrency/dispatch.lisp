(in-package :cl-bodge.concurrency.tests)

(in-suite :bodge-concurrency)


(defclass pooled-dispatcher ()
  ((executor :initform (make-pooled-executor 4))))

(defparameter *dispatcher* (make-instance 'pooled-dispatcher))

(defmethod dispatch ((this pooled-dispatcher) fn &key)
  (with-slots (executor) this
    (execute executor fn)))

(defun run-it (fn)
  (funcall fn *dispatcher* nil))

(define-flow serial-flow (a)
  (loop for i from 0 below 5
     collecting (let ((i i))
                  (-> :p ()
                    (+ a i)))))

(define-flow parallel-flow (a)
  (~> (loop for i from 0 below 3
         collecting (let ((i i))
                      (-> :p ()
                        (+ a i))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(test complex-flow
  (let (result)
    (flet ((put (v)
             (push v result)))
      (mt:wait-with-latch (latch)
        (run-it
         (>> (-> :g ()
               (put 0)
               1)
             (~> (-> :g (a)
                   (+ 1 a))
                 (-> :g (a)
                   (+ a 2))
                 (>> (-> :g (b)
                       (+ b 6))
                     (-> :g (b)
                       (+ b 7)))
                 (list (-> :g (a)
                         (+ a 3))
                       (-> :g (a)
                         (+ a 4))
                       (-> :g (a)
                         (values (+ a 5) -1))))
             (-> :g (a b c l)
               (destructuring-bind ((d) (e) (f g)) l
                 (put (list (car a) (car b) (car c) d e f g))))
             (list (parallel-flow 3)
                   (-> :g (r)
                     (put r)))
             (>> (serial-flow 1)
                 (-> :g (a)
                   (put a)))
             (-> :g ()
               (mt:open-latch latch))))))
    (is (equal '(0 (2 3 14 4 5 6 -1) ((3) (4) (5)) 5) (nreverse result)))))
