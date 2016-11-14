(in-package :cl-bodge.concurrency)


(defgeneric execute (obj fn))


(defmacro -> (place &body body)
  `(execute ,place (lambda () ,@body)))


(defmacro within-new-thread-waiting (thread-name &body body)
  (with-gensyms (latch)
    `(wait-with-latch (,latch)
       (bt:make-thread
        (lambda ()
          (unwind-protect
               (progn
                 ,@body)
            (open-latch ,latch))))
       :name ,(format nil "~a" thread-name))))
