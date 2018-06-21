(cl:in-package :cl-bodge.utils)

(defmacro definline (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))


(definline f (value)
  (float value 0f0))


(defmacro defenum (name &body values)
  (with-gensyms (value)
    (let ((enum-values-constant (symbolicate '+ name '-values+))
          (predicate (symbolicate name '-p)))
      `(progn
         (deftype ,name ()
           '(member ,@values))
         (define-constant ,enum-values-constant ',values :test #'equal)
         (declaim (ftype (function (*) (values boolean)) ,predicate)
                  (inline ,predicate))
         (defun ,predicate (,value)
           (not (null (member ,value ,enum-values-constant :test #'eql))))))))


(defmacro ensure-not-null (value)
  (once-only ((v value))
    `(if (null ,v)
         (error "Value of ~A must not be null" ',value)
         ,v)))


(definline if-null (value default)
  (if (null value)
      default
      value))


(defun apply-argument-list (lambda-list)
  (multiple-value-bind (required optional rest keywords)
      (parse-ordinary-lambda-list lambda-list)
    (let ((args (append required (mapcar #'first optional))))
      (if rest
          (append args (list rest))
          (append args (reduce #'nconc (mapcar #'car keywords)) (list nil))))))


(defmacro bound-symbol-value (symbol &optional default-value)
  `(if (boundp ',symbol) ,symbol ,default-value))


(defmacro if-bound (symbol-or-list then-form &optional else-form)
  (let ((symbols (ensure-list symbol-or-list)))
    `(if (and ,@(loop for symbol in symbols collect
                     `(boundp ',symbol)))
         ,then-form
         ,else-form)))


(defmacro when-bound (symbol-or-list &body body)
  `(if-bound ,symbol-or-list
             (progn ,@body)))


(definline class-name-of (obj)
  (class-name (class-of obj)))


(defun parse-initargs-and-list (initargs-and-list)
  (loop for (key . rest) on initargs-and-list by #'cddr
        until (listp key)
        append (list key (first rest)) into initargs
        finally (return (values initargs (unless (atom key)
                                           (append (list key) rest))))))


(defmacro bind-for-serious-condition ((handler) &body body)
  `(handler-bind ((serious-condition (lambda (c)
                                       (declare (ignore c))
                                       (funcall ,handler))))
     ,@body))
