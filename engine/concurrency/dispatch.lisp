(in-package :cl-bodge.concurrency)


(defgeneric dispatch (dispatcher task invariant &key &allow-other-keys))


(defmacro in-new-thread (thread-name &body body)
  "Execute `body` in the new thread with name `thread-name` instantly returning execution to the
 caller."
  `(bt:make-thread
    (lambda ()
      (progn
        ,@body))
    :name ,(format nil "~a" thread-name)))


(defmacro in-new-thread-waiting (thread-name &body body)
  "Execute `body` in the new thread with name `thread-name` blocking caller until `body`
returns."
  (with-gensyms (latch)
    `(wait-with-latch (,latch)
       (bt:make-thread
        (lambda ()
          (unwind-protect
               (progn
                 ,@body)
            (open-latch ,latch))))
       :name ,(format nil "~a" thread-name))))
