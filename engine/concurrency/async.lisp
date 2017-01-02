(in-package :cl-bodge.concurrency)


(defmacro *> (invariant-n-opts condition-var &body body)
  (declare (ignore invariant-n-opts condition-var body))
  (error "*> cannot be used outside of flow operator"))


(defun nop (result error-p)
  (declare (ignore result error-p)))


(declaim (ftype (function (* (or function null) * list function list) *) invariant-dispatch))
(defun invariant-dispatch (dispatcher result-callback invariant opts fn args)
  (labels ((return-error (e)
             (funcall result-callback (list e) t))
           (dispatched ()
             (handler-bind ((simple-error #'return-error))
               (funcall result-callback
                        (multiple-value-list (apply fn args)) nil))))
    (apply #'dispatch dispatcher #'dispatched :invariant invariant opts)))


(defun insert-rest-arg (lambda-list name)
  (multiple-value-bind (required optional rest key)
      (parse-ordinary-lambda-list lambda-list)
    (if rest
        (values lambda-list nil)
        (values (append required
                        (when optional
                          (append (list '&optional) optional))
                        (list '&rest name)
                        (when key
                          (append (list '&key) key)))
                t))))


(defmacro -> (invariant-n-opts lambda-list &body body)
  (destructuring-bind (invariant &rest opts) (ensure-list invariant-n-opts)
    (with-gensyms (dispatcher body-fn args result-callback rest-arg)
      (multiple-value-bind (new-lambda-list new-rest-p) (insert-rest-arg lambda-list rest-arg)
        `(lambda (,dispatcher ,result-callback &rest ,args)
           (declare (ignorable ,args))
           (flet ((,body-fn ,new-lambda-list
                    ,@(when new-rest-p
                        `((declare (ignore ,rest-arg))))
                    ,@body))
                    (invariant-dispatch ,dispatcher (or ,result-callback #'nop)
                                        ,invariant (list ,@opts)
                                        #',body-fn ,(when (not (null lambda-list)) args))))))))


(defun dispatch-list-flow (list dispatcher result-callback args)
  (labels ((dispatch-list (fn-list args)
             (flet ((dispatch-next (result error-p)
                      (if error-p
                          (log:error "Error during serial flow dispatch: ~A" result)
                          (dispatch-list (rest fn-list) result))))
               (if (null fn-list)
                   (funcall result-callback args nil)
                   (let ((flow-element (first fn-list)))
                     (if (listp flow-element)
                         (dispatch-list-flow flow-element dispatcher #'dispatch-next args)
                         (apply flow-element dispatcher #'dispatch-next args)))))))
    (dispatch-list list args)))


(defun dispatch-parallel-flow (list dispatcher result-callback args)
  (let ((n 0)
        (lock (make-recursive-lock "~>"))
        (flow-result (copy-tree list)))
    (labels ((count-elements (root)
               (if (listp root)
                   (loop for node in root summing (count-elements node))
                   1))
             (resolve (callback-list)
               (unless (null callback-list)
                 (let* ((element (car callback-list)))
                   (if (listp element)
                       (resolve element)
                       (flet ((%cons-result-callback (result error-p)
                                (when error-p
                                  (log:error "Error during parralel flow dispatch: ~A"
                                             result))
                                (setf (car callback-list) result)
                                (with-recursive-lock-held (lock) (decf n))
                                (when (= n 0)
                                  (funcall result-callback flow-result nil))))
                         (resolve (cdr callback-list))
                         (apply element dispatcher #'%cons-result-callback args)))))))
      (setf n (count-elements list))
      (resolve flow-result))))


(defmacro >> (&body flow)
  (with-gensyms (dispatcher result-callback args flow-tree)
    `(lambda (,dispatcher ,result-callback &rest ,args)
       (declare (type (or null (function (list t) *)) ,result-callback))
       (let ((,flow-tree (list ,@flow)))
         (dispatch-list-flow ,flow-tree ,dispatcher (or ,result-callback #'nop) ,args)))))


(defmacro define-flow (name (&rest lambda-list) &body body)
  `(defun ,name ,lambda-list
     (>> ,@body)))


(defmacro ~> (&body body)
  (with-gensyms (dispatcher args result-callback flow)
    `(lambda (,dispatcher ,result-callback &rest ,args)
       (declare (type (or (function (list t) *) null) ,result-callback))
       (let ((,flow (list ,@body)))
         (dispatch-parallel-flow ,flow ,dispatcher (or ,result-callback #'nop) ,args)))))
