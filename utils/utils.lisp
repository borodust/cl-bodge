(in-package :cl-bodge.utils)

(defmacro log-errors (&body body)
  `(handler-case
       (progn ,@body)
     (t (e) (log:error "Unhandled error: ~a" e))))


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


;; alexandria paste
(defun stream->byte-array (stream &key (initial-size 4096))
  (check-type initial-size positive-integer)
  (do ((buffer (make-array initial-size :element-type (stream-element-type stream)))
       (offset 0)
       (offset-wanted 0))
      ((/= offset-wanted offset)
       (if (= offset (length buffer))
           buffer
           (subseq buffer 0 offset)))
    (unless (zerop offset)
      (let ((new-buffer (make-array (* 2 (length buffer))
                                    :element-type (stream-element-type stream))))
        (replace new-buffer buffer)
        (setf buffer new-buffer)))
    (setf offset-wanted (length buffer)
          offset (read-sequence buffer stream :start offset))))


(declaim (inline file->byte-array))
(defun file->byte-array (pathname &optional (element-type '(unsigned-byte 8)))
  (with-input-from-file (stream pathname :element-type element-type)
    (stream->byte-array stream)))


(defmacro defenum (name &body values)
  (with-gensyms (value)
    (let ((enum-values-constant (symbolicate '+ name '-values+))
          (predicate (symbolicate name '-p)))
      `(progn
         (deftype ,name ()
           '(member ,@values))
         (define-constant ,enum-values-constant ',values :test #'equal)
         (declaim (ftype (function (,name) boolean) ,predicate)
                  (inline ,predicate))
         (defun ,predicate (,value)
           (not (null (member ,value ,enum-values-constant :test #'eql))))))))
