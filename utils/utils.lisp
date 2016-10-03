(in-package :cl-bodge.utils)

(defmacro log-errors (&body body)
  `(handler-case
       (progn ,@body)
     (t (e) (log:error "Unhandled error: ~a" e))))


(defun read-file-into-string-list (pathname)
  (split-sequence:split-sequence "#\Newline" (read-file-into-string pathname)))


(defmacro with-hash-entries ((&rest keys) hash-table &body body)
  (once-only (hash-table)
    `(symbol-macrolet (,@(loop for key in keys collecting
			      (if (listp key)
				  (destructuring-bind (key-name key-value) key
				    `(,key-name (gethash ,key-value ,hash-table)))
				  `(,key (gethash ,(make-keyword key) ,hash-table)))))
       ,@body)))


(defmacro make-hash-table-with-entries ((&rest keys) (&rest initargs) &body body)
  (with-gensyms (table)
    `(let ((,table (make-hash-table ,@initargs)))
       (with-hash-entries (,@keys) ,table
	 ,@body)
       ,table)))
