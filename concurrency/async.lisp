(in-package :cl-bodge.concurrency)


(defmacro when-all ((&rest promise-gens) &body body)
  `(alet ,(loop for promise in promise-gens collecting
               `(nil ,promise))
     ,@body))


(defmacro when-all* ((&rest promise-gens) &body body)
  `(alet* ,(loop for promise in promise-gens collecting
               `(nil ,promise))
     ,@body))


(defmacro wait-let ((&rest bindings) &body body)
  (with-gensyms (latch condition c)
    (let ((gensymed (loop for b in bindings
                       for (name value) = (if (atom b)
                                              (list b nil)
                                              (if (null (rest b))
                                                  (append b nil)
                                                  b))
                       collect (list name (gensym (symbol-name name))
                                     (gensym (symbol-name name)) value))))
      `(let (,@(loop for b in gensymed collect (second b))
             ,condition)
         (wait-with-latch (,latch)
           (finally
               (catcher
                (alet (,@(loop for b in gensymed collect (rest (rest b))))
                  ,@(loop for b in gensymed
                       for (g1 . g2) = (rest b) collect
                         `(setf ,g1 ,(first g2))))
                (t (,c) (setf ,condition ,c)))
             (open-latch ,latch)))
         (unless (null ,condition)
           (error ,condition))
         (let (,@(loop for (name . g1) in gensymed collect
                      (list name (first g1))))
           ,@body)))))


(defun wait (promise)
  (wait-let ((r promise)) r))
