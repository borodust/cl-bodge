(in-package :cl-bodge.concurrency)


(defgeneric execute (obj fn &optional priority))


(defmacro -> ((place &optional (priority :medium)) &body body)
  `(execute ,place (lambda () ,@body) ,priority))


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
