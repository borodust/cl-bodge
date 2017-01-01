(in-package :cl-bodge.concurrency)


(defgeneric dispatch (dispatcher task &key &allow-other-keys)
  (:method (dispatcher task &key &allow-other-keys)
    nil))


(defmethod dispatch :around (dispatcher (fn function) &rest keys &key)
  (flet ((wrapped ()
           (let ((*active-dispatcher* dispatcher))
             (funcall fn))))
    (apply #'call-next-method dispatcher #'wrapped keys)))


(defmacro in-new-thread (thread-name &body body)
  `(bt:make-thread
    (lambda ()
      (progn
        ,@body))
    :name ,(format nil "~a" thread-name)))


(defmacro in-new-thread-waiting (thread-name &body body)
  (with-gensyms (latch)
    `(wait-with-latch (,latch)
       (bt:make-thread
        (lambda ()
          (unwind-protect
               (progn
                 ,@body)
            (open-latch ,latch))))
       :name ,(format nil "~a" thread-name))))
