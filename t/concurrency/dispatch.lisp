(in-package :cl-bodge.concurrency.tests)

(in-suite :bodge-concurrency)

(defmacro %body (&body body &environment env)
  (ge.mt::traverse-dispatch-body `(progn ,@body) env))


(defclass dummy-dispatcher ()())

(defmethod ge.mt::dispatch ((this dummy-dispatcher) fn &key)
  (funcall fn))

(defvar *dummy-dispatcher* (make-instance 'dummy-dispatcher))


(defclass simple-dispatcher (ge.mem:disposable)
  ((e :initform (ge.mt:make-single-threaded-executor))))

(ge.mem:define-destructor simple-dispatcher (e)
  (ge.mem:dispose e))

(defmethod ge.mt::dispatch ((this simple-dispatcher) fn &key (priority :medium))
  (with-slots (e) this
    (ge.mt:execute e fn :priority priority)))


(ge.mt:defun/d foo/d (d)
  (let ((result 1))
    (ge.mt:wait-for (ge.mt:-> (d)
                      (incf result)))
    result))


(ge.mt:defun/d bar/d ()
  (let ((r0 3)
        (r1 3)
        result)
    (ge.mt:wait-for (ge.mt::-> ((make-instance 'simple-dispatcher))
                      (setf r0 0)))
    (setf r1 (foo/d *dummy-dispatcher*)
          result (+ r0 r1))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test symbol-macrolet-body-traverse
  (is (equal '(0 1)
             (let ((result '()))
               (%body
                 (push 0 result)
                 (symbol-macrolet ((x 1))
                   (ge.mt::wait-for)
                   (push x result)))
               (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test progn-body-traverse
  (is (equal '(0 2 3 1 4 5 6 7)
             (let ((result '()))
               (%body
                 (push 0 result)
                 (progn
                   (push 2 result)
                   (push 3 result)
                   (progn
                     (push 1 result)
                     (progn
                       (push 4 result)
                       (ge.mt::wait-for)
                       (push 5 result)
                       (push 6 result))
                     (push 7 result))))
               (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test let-body-traverse
  (is (equal '(0 2 3 1 4 5 6 7)
             (let ((result '()))
               (%body
                 (push 0 result)
                 (let ()
                   (push 2 result)
                   (push 3 result)
                   (let ((a 7))
                     (push 1 result)
                     (let ((b 4)
                           (c 6))
                       (push b result)
                       (ge.mt::wait-for)
                       (push 5 result)
                       (push c result))
                     (push a result)))
                 (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test dummy-dispatching
  (is (equal '(0 3 1 5 4 6 7 2)
             (let ((d (make-instance 'dummy-dispatcher))
                   result)
               (ge.mt:-> (d)
                 (push 0 result)
                 (progn
                   (let ((a 6))
                     (push 3 result)
                     (ge.mt:wait-for (ge.mt:-> (d)
                                       (push 1 result))
                                     (ge.mt:-> (d)
                                       (push 5 result)))
                     (push 4 result)
                     (push a result))
                   (push 7 result))
                 (push 2 result))
               (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test block-body-traverse
  (is (equal '(0 2 3 7 4)
             (let ((ge.mt::*active-dispatcher* *dummy-dispatcher*)
                   (result '()))
               (%body
                 (push 0 result)
                 (block nil
                   (push 2 result)
                   (push 3 result)
                   (ge.mt::wait-for)
                   (push 7 result))
                 (push 4 result))
               (nreverse result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test body-w/o-async-forms
  (let (result)
    (ge.mt:-> (*dummy-dispatcher*)
      (push 0 result)
      (progn
        (let ((a 6))
          (push 3 result)
          (push 1 result)
          (push 5 result)
          (push 4 result)
          (push a result))
        (push 7 result))
      (push 2 result))

    (is (equal '(0 3 1 5 4 6 7 2) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test body-w-multiple-async-forms
  (let (result)
    (ge.mt:-> (*dummy-dispatcher*)
      (push 0 result)
      (progn
        (push 3 result)
        (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*)
                          (push 8 result)))
        (push 1 result)
        (push 5 result)
        (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*)
                          (push 9 result)))
        (push 4 result)
        (push 7 result))
      (push 2 result))

    (is (equal '(0 3 8 1 5 9 4 7 2) (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test block-return
  (is (equal '(0 3 9 8 11 5 11)
             (let ((d (make-instance 'dummy-dispatcher))
                   result)
               (ge.mt:-> (d)
                 (push 0 result)
                 (block foo
                   (push 3 result)
                   (ge.mt:wait-for (ge.mt:-> (d)
                                     (push 9 result))
                                   (ge.mt:-> (d)
                                     (push 8 result)
                                     (return-from foo)
                                     (push 10 result))
                                   (ge.mt:-> (d)
                                     (push 5 result)
                                     (return-from foo)))
                   (push 2 result))
                 (push 11 result))
               (nreverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test multithreaded-concurrent-dispatch
  (let ((d0 (make-instance 'simple-dispatcher))
        (d1 (make-instance 'simple-dispatcher))
        result
        threads)
    (mt:wait-with-latch (latch)
      (ge.mt:-> (d0)
        (push (bt:current-thread) threads)
        (push 0 result)
        (progn
          (let ((a 6))
            (push 3 result)
            (ge.mt:wait-for (ge.mt:-> (d1)
                              (push 1 result))
                            (ge.mt:-> (d1)
                              (push 5 result)))
            (push (bt:current-thread) threads)
            (push 4 result)
            (push a result))
          (push 7 result))
        (push 2 result)
        (mt:open-latch latch)))
    (setf result (nreverse result))

    (is (equal '(0 3 1 5 4 6 7 2) result))
    (is (eq (first threads) (second threads)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test multithreaded-serial-dispatch
  (let ((d0 (make-instance 'simple-dispatcher))
        (d1 (make-instance 'simple-dispatcher))
        (d2 (make-instance 'simple-dispatcher))
        result
        threads)
    (mt:wait-with-latch (latch)
      (ge.mt:-> (d0)
        (push (bt:current-thread) threads)
        (push 0 result)
        (progn
          (let ((a 6))
            (push 3 result)
            (ge.mt:wait-for* (ge.mt:-> (d1)
                               (push 1 result))
                             (ge.mt:-> (d2)
                               (push 5 result))
                             (ge.mt:-> (d1)
                               (push 8 result))
                             (ge.mt:-> (d2)
                               (push 9 result)))
            (push (bt:current-thread) threads)
            (push 4 result)
            (push a result))
          (push 7 result))
        (push 2 result)
        (mt:open-latch latch)))
    (setf result (nreverse result))

    (is (equal '(0 3 1 5 8 9 4 6 7 2) result))
    (is (eq (first threads) (second threads)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test multithreaded-serial-direct-dispatch
  (let ((d0 (make-instance 'simple-dispatcher))
        (d1 (make-instance 'simple-dispatcher))
        (d2 (make-instance 'simple-dispatcher))
        result)
    (mt:wait-with-latch (latch)
      (ge.mt:-> (d0)
        (ge.mt:wait-for* (ge.mt:-> (d1)
                           (push 1 result))
                         (ge.mt:-> (d2)
                           (push 5 result))
                         (ge.mt:-> (d1)
                           (push 8 result))
                         (ge.mt:-> (d2)
                           (push 9 result)
                           (mt:open-latch latch)))))
    (setf result (nreverse result))

    (is (equal '(1 5 8 9) result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test setf-dispatch
  (let (a b c d e f g h)
    (ge.mt:-> (*dummy-dispatcher*)
      (setf a 0
            b (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*) 1)
                              (ge.mt:-> (*dummy-dispatcher*) 8))
            c 2)

      (setf d (ge.mt:wait-for* (ge.mt:-> (*dummy-dispatcher*) 3))
            e 4
            f (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*) 5)))

      (setf g (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*) 6)))
      (setf h 7))

    (is (equal '(0 (1 8) 2 3 4 5 6 7) (list a b c d e f g h)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test defun/d-call
  (let (result)

    (mt:wait-with-latch (l)
      (ge.mt::-> (*dummy-dispatcher*)
        (setf result (foo/d (make-instance 'simple-dispatcher)))
        (mt:open-latch l)))

    (is (equal 2 result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test defun/d-nested-call
  (let (result)

    (mt:wait-with-latch (l)
      (ge.mt::-> (*dummy-dispatcher*)
        (setf result (bar/d))
        (mt:open-latch l)))

    (is (equal 2 result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test dolist-dispatch
  (let ((result nil)
        (list '(0 1 2 3 4)))

    (ge.mt:-> (*dummy-dispatcher*)
      (dolist (i list)
        (ge.mt:wait-for (ge.mt:-> (*dummy-dispatcher*)
                          (push i result)))))

    (is (equal list (nreverse result)))))
