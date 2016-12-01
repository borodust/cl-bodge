(in-package :cl-bodge.concurrency)


(defclass null-cont (continuation) ())


(defclass fn-name-cont (continuation)
  ((fn-name :initarg :function-name :reader function-name-for)))


(defclass fn-name-null-cont (fn-name-cont null-cont) ())

(defmethod generate-calling-code ((this fn-name-null-cont) result)
  `(,(function-name-for this)))

(defun make-function-name-null-continuation (fn-name)
  (make-instance 'fn-name-null-cont :function-name fn-name))


(defclass result-cont (continuation) ())


(defclass fn-name-result-cont (fn-name-cont result-cont) ())

(defmethod generate-calling-code ((this fn-name-result-cont) result)
  `(,(function-name-for this) ,result))

(defun make-function-name-result-continuation (fn-name)
  (make-instance 'fn-name-result-cont :function-name fn-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric form-list-transform (codegen group)
  (:method (codegen group) group))

(defgeneric plain-cc-gen (codegen cont prologue))

(defmethod plain-cc-gen (codegen (cont null) prologue)
  `(,@(form-list-transform codegen prologue)))

(defmethod plain-cc-gen (codegen (cont null-cont) prologue)
  `(,@(form-list-transform codegen prologue)
    ,(generate-calling-code cont nil)))

(defmethod plain-cc-gen (codegen (cont result-cont) prologue)
  (with-gensyms (r)
    `((let ((,r ,@(form-list-transform codegen prologue)))
        ,(generate-calling-code cont r)))))


(defgeneric cc-callback-gen (codegen cont cb epilogue))

(defmethod cc-callback-gen (codegen (cont null) cb epilogue)
  `(,cb () ,@(form-list-transform codegen epilogue)))


(defmethod cc-callback-gen (codegen (cont null-cont) cb epilogue)
  `(,cb ()
        ,@(form-list-transform codegen epilogue)
        ,(generate-calling-code cont nil)))

(defmethod cc-callback-gen (codegen (cont result-cont) cb epilogue)
  (with-gensyms (r)
    `(,cb (,r)
          ,@(when epilogue
              `((setf ,r (progn ,@(form-list-transform codegen epilogue)))))
          ,(generate-calling-code cont r))))

(defgeneric chain-callback-body-gen (codegen group last-callback result-required-p)
  (:method (codegen group last-callback result-required-p)
    `(,@(rest group)
        ,(generate-code (car group)
                        (if result-required-p
                            (make-function-name-result-continuation last-callback)
                            (make-function-name-null-continuation last-callback))))))

(defgeneric chain-callback-gen (codegen group callback-name last-callback result-required-p)
  (:method (codegen group callback-name last-callback result-required-p)
    `(,callback-name
      () ,@(chain-callback-body-gen codegen group last-callback result-required-p))))


(defgeneric chain-calling-code-gen (codegen group last-callback result-required-p)
  (:method (codegen group last-callback result-required-p)
    (chain-callback-body-gen codegen group last-callback result-required-p)))


(defun split-into-groups (forms &key (test #'codegen-p))
  (loop with prologue
     for form in forms
     if (funcall test form) collect
       (prog1 (cons form (nreverse prologue))
         (setf prologue nil))
       into groups
     else do
       (push form prologue)
     finally (return (values groups (nreverse prologue)))))


(defun generate-callbacks (codegen groups cb result-required-p)
  (loop with cur-cb = cb and return-result-p = result-required-p
     for group in (reverse groups)
     for next-cb = (gensym "NEXT-CB")
     collect
       (prog1 (chain-callback-gen codegen group next-cb cur-cb (and (eq cur-cb cb)
                                                                    return-result-p))
         (setf cur-cb next-cb))
     into callbacks
     finally (return (values callbacks cur-cb))))


(defun generate-callback-chain (codegen forms cont
                                &key (codegen-test #'codegen-p))
  (let ((result-required-p (subtypep (class-of cont) 'result-cont)))
    (multiple-value-bind (groups epilogue) (split-into-groups forms :test codegen-test)
      (if (null groups)
          (plain-cc-gen codegen cont epilogue)
          (with-gensyms (cb)
            (multiple-value-bind (callbacks last-cb)
                (generate-callbacks codegen (rest groups) cb result-required-p)
              `((labels (,(cc-callback-gen codegen cont cb epilogue)
                         ,@callbacks)
                  ,@(chain-calling-code-gen codegen (car groups) last-cb
                                            (and result-required-p (null callbacks)))))))))))


(defun traverse-list (forms)
  (loop for form in forms
     if (listp form) collect (traverse-form (car form) form)
     else collect form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass setf-gen (code-generator) ())

(defmethod chain-callback-body-gen ((this setf-gen) group last-cb result-required-p)
  (with-gensyms (cb r)
    `(,@(form-list-transform this (rest group))
      (flet ((,cb (,r)
               (setf ,(caar group) ,r)
               (,last-cb ,@(when result-required-p `(,r)))))
        ,(generate-code (cdar group)
                        (make-function-name-result-continuation cb))))))

(defmethod form-list-transform ((this setf-gen) forms)
  (when-let ((plist (loop for (var . val) in forms appending (list var val))))
    `((setf ,@plist))))

(defmethod generate-code ((this setf-gen) cont)
  (let* ((pairs (loop for (var . val) in (plist-alist (rest (form-of this))) collect
                     (cons var (if (listp val)
                                   (traverse-form (car val) val)
                                   val))))
         (chain (generate-callback-chain this pairs cont
                                  :codegen-test (lambda (pair) (codegen-p (cdr pair))))))

    (if (null (rest chain))
        (first chain)
        `(progn
           ,@chain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass execution-list-gen (code-generator) ())

(defgeneric execution-list-of (code-gen form))

(defun collect-traversed (forms)
  (loop for form in forms collect
       (if (listp form)
           (traverse-form (first form) form)
           form)))

(defmethod generate-code ((this execution-list-gen) cont)
  (let* ((forms (execution-list-of this (collect-traversed (form-of this)))))
    (generate-callback-chain this forms cont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass block-gen (execution-list-gen) ())

(defmethod execution-list-of ((this block-gen) form)
  (cddr form))


(defmacro in-block ((block-name return-funame (this)) &body body)
  `(let* ((,block-name (second (form-of ,this)))
          (,return-funame (make-funame/d ,block-name 'block)))
     `(block ,block-name
        ,,@body)))


(defmethod generate-code ((this block-gen) (cont null))
  (in-block (block-name return-funame (this))
    (with-gensyms (r)
      `(flet ((,return-funame (&optional ,r)
                (declare (ignore ,r))))
         ,@(call-next-method)))))


(defmethod generate-code ((this block-gen) (cont null-cont))
  (in-block (block-name return-funame (this))
    (with-gensyms (r d)
      `(let ((,d *active-dispatcher*))
         (flet ((,return-funame (&optional ,r)
                  (declare (ignore ,r))
                  ;; fixme : propagate priority
                  (dispatch ,d (lambda () ,(generate-calling-code cont nil)) :medium)))
           ,@(call-next-method this (make-function-name-null-continuation return-funame)))))))


(defmethod generate-code ((this block-gen) (cont result-cont))
  (in-block (block-name return-funame (this))
    (with-gensyms (r d)
      `(let ((,d *active-dispatcher*))
         (flet ((,return-funame (&optional ,r)
                  ;; fixme : propagate priority
                  (dispatch ,d (lambda () ,(generate-calling-code cont r)) :medium)))
           ,@(call-next-method this
                               (make-function-name-result-continuation return-funame)))))))


(defclass return-from-gen (code-generator) ())


(defmethod generate-code ((this return-from-gen) cont)
  (let* ((form (form-of this))
         (cb-name (make-funame/d (second form) 'block)))
    (when-bound *active-callback-name*
      (setf *active-callback-name* nil))
    `(,cb-name ,(third form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass progn-gen (execution-list-gen) ())

(defmethod execution-list-of ((this progn-gen) form)
  (rest form))

(defmethod generate-code ((this progn-gen) cont)
  (let ((list (call-next-method)))
    (if (null (rest list))
        (first list)
        `(progn ,@list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass let-gen (execution-list-gen) ())

(defmethod execution-list-of ((this let-gen) form)
  (cddr form))

(defmethod generate-code ((this let-gen) cont)
  `(let ,(second (form-of this)) ,@(call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass let*-gen (execution-list-gen) ())

(defmethod execution-list-of ((this let*-gen) form)
  (cddr form))

(defmethod generate-code ((this let*-gen) cont)
  `(let* ,(second (form-of this)) ,@(call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fn/d-gen (code-generator) ())

(defmethod generate-code ((this fn/d-gen) (cont null))
  (let* ((form (form-of this))
         (funame (first form)))
    `(,(make-funame/d funame) nil ,@(rest form))))

(defmethod generate-code ((this fn/d-gen) (cont null-cont))
  (let* ((form (form-of this))
         (funame (first form)))
    (with-gensyms (cb r)
      `(flet ((,cb (,r)
                (declare (ignore ,r))
                ,(generate-calling-code cont nil)))
         (,(make-funame/d funame) #',cb ,@(rest form))))))

(defmethod generate-code ((this fn/d-gen) (cont result-cont))
  (let* ((form (form-of this))
         (funame (first form)))
    (with-gensyms (cb r)
      `(flet ((,cb (,r)
                ,(generate-calling-code cont r)))
         (,(make-funame/d funame) #',cb ,@(rest form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dolist-gen (code-generator) ())


(defmethod generate-code ((this dolist-gen) cont)
  (let* ((form (form-of this))
         (traversed (traverse-list (cddr form))))
    (destructuring-bind (var list-form &optional result-form) (second form)
      (with-gensyms (%dolist %next-iter list)
        (let ((chain (generate-callback-chain
                      this traversed
                      (make-function-name-null-continuation %next-iter))))
          `(labels ((,%dolist (,list)
                      (flet ((,%next-iter () (,%dolist (rest ,list))))
                          (if (null ,list)
                              ,(generate-calling-code cont result-form)
                              (let ((,var (car ,list)))
                                ,@chain)))))
             (,%dolist ,list-form)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass wait-for-gen (code-generator) ())

(defmethod generate-code ((this wait-for-gen) (cont null))
  `(progn ,@(rest (form-of this))))


(defmacro with-dispatching ((forms count (this cont)) &body body)
  (once-only (this cont)
    `(let* ((,forms (rest (form-of ,this)))
            (,count (length ,forms)))
       (if (> ,count 0)
           (progn ,@body)
           (generate-calling-code ,cont nil)))))


(defmethod generate-code ((this wait-for-gen) (cont null-cont))
  (with-dispatching (forms count (this cont))
    (with-gensyms (cb n r d)
      (let ((*active-callback-name* cb))
        `(let ((,d *active-dispatcher*))
           ,(if (null (rest forms))
                `(flet ((,cb (,r)
                          (declare (ignore ,r))
                          ;; fixme : propagate priority
                          (dispatch ,d #'(lambda () ,(generate-calling-code cont nil)) :medium)))
                   ,(macroexpand-1 (car forms) *env*))
                `(let ((,n ,count))
                   (flet ((,cb (,r)
                            (declare (ignore ,r))
                            (dispatch ,d
                                      #'(lambda ()
                                          (decf ,n)
                                          (when (= ,n 0)
                                            ,(generate-calling-code cont nil)))
                                      :medium))) ; fixme : propagate priority
                     ,@(loop for form/d in forms collect
                            (macroexpand-1 form/d *env*))))))))))


(defmethod generate-code ((this wait-for-gen) (cont result-cont))
  (with-dispatching (forms count (this cont))
    (with-gensyms (cb n r results d)
      (let ((*active-callback-name* cb))
        `(let ((,d *active-dispatcher*))
           ,(if (null (rest forms))
                `(flet ((,cb (,r)
                          ;; fixme : propagate priority
                          (dispatch ,d #'(lambda () ,(generate-calling-code cont r)) :medium)))
                   ,(macroexpand-1 (car forms) *env*))
                `(let ((,n ,count)
                       (,results '()))
                   (flet ((,cb (,r)
                            (dispatch ,d
                                      #'(lambda ()
                                          (decf ,n)
                                          (push ,r ,results)
                                          (when (= ,n 0)
                                            ,(generate-calling-code cont `(nreverse ,results))))
                                      :medium))) ; fixme : propagate priority
                     ,@(loop for form/d in forms collect
                            (macroexpand-1 form/d *env*))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass wait-for*-gen (code-generator)
  ((active-dispatcher-name :initform nil :accessor active-dispatcher-name-for)))


(defmethod cc-callback-gen ((codegen wait-for*-gen) (cont null) cb epilogue)
  (with-gensyms (r)
    `(,cb (,r) (declare (ignore ,r)))))

(defmethod cc-callback-gen ((codegen wait-for*-gen) (cont null-cont) cb epilogue)
  (with-gensyms (r)
    `(,cb (,r)
          (declare (ignore ,r))
          ;; fixme : propagate priority
          (dispatch ,(active-dispatcher-name-for codegen)
                    #'(lambda () ,(generate-calling-code cont nil)) :medium))))

(defmethod cc-callback-gen ((codegen wait-for*-gen) (cont result-cont) cb epilogue)
  (with-gensyms (r)
    `(,cb (,r)
          (when (null (rest ,r))
            (setf ,r (car ,r)))
          ;; fixme : propagate priority
          (dispatch ,(active-dispatcher-name-for codegen)
                    #'(lambda () ,(generate-calling-code cont r)) :medium))))


(defmethod chain-callback-gen ((this wait-for*-gen) group name last-cb result-required-p)
  (with-gensyms (cb cb-r r)
    `(,name (,r)
            (flet ((,cb (,cb-r)
                     (,last-cb (cons ,cb-r ,r))))
              ,(let ((*active-callback-name* cb))
                 (macroexpand-1 (car group) *env*))))))


(defmethod chain-calling-code-gen ((this wait-for*-gen) group last-cb result-required-p)
  (with-gensyms (cb r)
    `((flet ((,cb (,r)
               (,last-cb (list ,r))))
        ,(let ((*active-callback-name* cb))
           (macroexpand-1 (car group) *env*))))))


(defmethod form-list-transform ((this wait-for*-gen) forms)
  nil)

(defmethod generate-code ((this wait-for*-gen) cont)
  (with-gensyms (d)
    (setf (active-dispatcher-name-for this) d)
    (let ((chain (generate-callback-chain this (rest (form-of this)) cont
                                          :codegen-test (constantly t))))
      `(let ((,d *active-dispatcher*))
         ,@chain))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod traverse-form (type form)
  (declare (special *env*))
  (multiple-value-bind (expansion expanded-p) (macroexpand-1 form *env*)
    (if expanded-p
        (traverse-form (car expansion) expansion)
        expansion)))


(defmethod traverse-form ((type (eql 'progn)) form)
  (make-instance 'progn-gen :form form))


(defmethod traverse-form ((type (eql 'let)) form)
  (make-instance 'let-gen :form form))


(defmethod traverse-form ((type (eql 'let*)) form)
  (make-instance 'let*-gen :form form))


(defmethod traverse-form ((type (eql 'block)) form)
  (make-instance 'block-gen :form form))


(defmethod traverse-form ((type (eql 'return-from)) form)
  (make-instance 'return-from-gen :form form))


(defmethod traverse-form ((type (eql 'return)) form)
  (make-instance 'return-from-gen :form `(return-from nil ,(second form))))


(defmethod traverse-form ((type (eql 'wait-for)) form)
  (make-instance 'wait-for-gen :form form))


(defmethod traverse-form ((type (eql 'wait-for*)) form)
  (make-instance 'wait-for*-gen :form form))


(defmethod traverse-form ((type (eql 'setf)) form)
  (make-instance 'setf-gen :form form))


(defmethod traverse-form ((type (eql 'dolist)) form)
  (make-instance 'dolist-gen :form form))
