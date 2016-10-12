(in-package :cl-bodge.concurrency)


(defmacro when-all ((&rest promise-gens) &body body)
  `(alet ,(loop for promise in promise-gens collecting
               `(nil ,promise))
     ,@body))


(defmacro when-all* ((&rest promise-gens) &body body)
  `(alet* ,(loop for promise in promise-gens collecting
               `(nil ,promise))
     ,@body))
