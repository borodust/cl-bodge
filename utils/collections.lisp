(cl:in-package :cl-bodge.utils)


(defmacro with-hash-entries ((&rest keys) hash-table &body body)
  (once-only (hash-table)
    (multiple-value-bind (lets mlets)
        (loop for key in keys
           for (val-name key-name key-value) = (if (listp key)
                                                            (append key (list (gensym)))
                                                            (list key key (gensym)))
           collecting `(,val-name (gethash ,key-value ,hash-table)) into mlets
           collecting `(,key-value ,key-name) into lets
           finally (return (values lets mlets)))
      `(let ,lets
         (symbol-macrolet ,mlets
           ,@body)))))


(defmacro make-hash-table-with-entries ((&rest initargs) &body pairs)
  (with-gensyms (table)
    `(let ((,table (make-hash-table ,@initargs)))
       ,@(loop for (key value) in pairs
               collect `(setf (gethash ,key ,table) ,value))
       ,table)))


(defun list->array (list &rest dimensions)
  (let ((element (if (null dimensions)
                     (car list)
                     (loop repeat (length dimensions)
                        for el = list then (car el)
                        finally (return (car el)))))
        (dimensions (or dimensions (length list))))
    (typecase element
      (integer
       (make-array dimensions :element-type 'integer
                   :initial-contents list))
      (single-float
       (make-array dimensions :element-type 'single-float
                   :initial-contents list)))))

(defun search-sorted (value sorted-array &key (test #'eql) (predicate #'<) (key #'identity))
  (labels ((%aref (idx)
             (aref sorted-array idx))
           (%compare (idx)
             (funcall predicate value (funcall key (%aref idx))))
           (%test (idx)
             (funcall test value (funcall key (%aref idx))))
           (%search (start end)
             (if (= start end)
                 (values nil end)
               (let ((idx (floor (/ (+ start end) 2))))
                 (if (%test idx)
                     (values (%aref idx) idx)
                     (if (%compare idx)
                         (%search start idx)
                         (%search (1+ idx) end)))))))
    (%search 0 (length sorted-array))))
