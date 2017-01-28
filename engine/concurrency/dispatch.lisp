(in-package :cl-bodge.concurrency)


(declaim (special *active-dispatcher*))


(defmethod dispatch (dispatcher fn &key)
  nil)


(defmethod dispatch :around (dispatcher fn &rest keys &key)
  (flet ((wrapped ()
           (let ((*active-dispatcher* dispatcher))
             (funcall fn))))
    (apply #'call-next-method dispatcher #'wrapped keys)))


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
