(in-package :cl-bodge.concurrency)


(declaim (special *active-dispatcher*
                  *active-callback-name*
                  *env*))


(defgeneric dispatch (dispatcher task &optional priority))

(defmethod dispatch :around (dispatcher (fn function) &optional priority)
  (call-next-method dispatcher
                    (lambda ()
                      (let ((*active-dispatcher* dispatcher))
                        (funcall fn)))
                    priority))


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


(defun wait-for (&rest dispatching-forms)
  (declare (ignore dispatching-forms))
  (error "'wait-for' can be used inside dispatchable environment only"))


(defun wait-for* (&rest dispatching-forms)
  (declare (ignore dispatching-forms))
  (error "'wait-for*' can be used inside dispatchable environment only"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass continuation () ())

(defgeneric generate-calling-code (continuation result)
  (:method ((cont null) result) nil))


(defclass code-generator ()
  ((form :initarg :form :reader form-of)))

(defgeneric generate-code (generator parent-cont))

(defun codegen-p (o)
  (subtypep (class-of o) 'code-generator))


(defgeneric traverse-form (type form))


(defun traverse-dispatch-body (body env &optional cont)
  (let ((*env* env))
    (declare (special *env*))
    (let ((form (traverse-form (car body) body)))
      (if (codegen-p form)
          (generate-code form cont)
          form))))


(defun make-funame/d (name &optional (prefix ""))
  (format-symbol :ge.mt.gen "~a_~a_~a/D" prefix
                 (package-name (symbol-package name)) name))


(defmacro -> (&environment env (dispatcher &optional (priority :medium)) &body body)
  (with-gensyms (fn r)
    (let ((transformed (traverse-dispatch-body `(progn ,@body) env)))
      `(flet ((,fn ()
                ,(if-bound *active-callback-name*
                           (if *active-callback-name*
                               `(let (,r)
                                  (unwind-protect
                                       (setf ,r ,transformed)
                                    (funcall #',*active-callback-name* ,r)))
                               transformed)
                           transformed)))
         (dispatch ,dispatcher #',fn ,priority)))))
