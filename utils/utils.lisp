(in-package :cl-bodge.utils)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (ftype (function (*) single-float) f)
           (inline f))
  (defun f (obj)
    (coerce obj 'single-float))

  (set-dispatch-macro-character #\# #\f
                                (lambda (stream key arg)
                                  (declare (ignore key arg))
                                  (let ((sexp (read stream t nil t)))
                                    (if (numberp sexp)
                                        (f sexp)
                                        `(ge.util:f ,sexp))))))


(defmacro log-errors (&body body)
  `(handler-case
       (progn ,@body)
     (t (e) (log:error "Unhandled error: ~a" e))))


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


(defun epoch-seconds-of (timestamp)
  (+ (timestamp-to-unix timestamp) (/ (nsec-of timestamp) 1000000000)))


(defmacro definline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))


(cffi:defcfun ("memcpy" %copy-memory) :pointer
  (destination :pointer)
  (source :pointer)
  (size :int))


(defun copy-memory (destination source type &optional (count 1))
  (%copy-memory destination source (* (cffi:foreign-type-size type) count)))


(defmacro ensure-not-null (value)
  (once-only ((v value))
    `(if (null ,v)
         (error "Value of ~a must not be null" ',value)
       ,v)))


(defmacro if-unbound (symbol value)
  `(if (boundp ',symbol) ,symbol ,value))
