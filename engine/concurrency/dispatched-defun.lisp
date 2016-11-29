(in-package :cl-bodge.concurrency)


(defmacro defun/d (&whole whole name lambda-list &body body &environment env)
  (with-gensyms (cont cb r)
    (let ((name/d (make-funame/d name)))
      (multiple-value-bind (required optional rest key)
          (parse-ordinary-lambda-list lambda-list)
        (multiple-value-bind (forms decls doc) (parse-body body :documentation t :whole whole)
          `(progn
             (defmethod traverse-form ((type (eql ',name)) form)
               (make-instance 'fn/d-gen :form form))

             (defun ,name/d ,(cons cont lambda-list)
               ,@decls
               (flet ((,cb (,r)
                        (when ,cont (funcall ,cont ,r))))
                 ,(traverse-dispatch-body `(block ,name ,@forms) env
                                          (make-function-name-result-continuation cb))))

             (definline ,name ,lambda-list
               ,@(when doc
                   (list doc))
               (declare (ignore ,@required
                                ,@(mapcar #'cadr optional)
                                ,@(when rest (list rest))
                                ,@(mapcar #'cadar key)))
               (error "Function ~a cannot be called outside of dispatched environment"
                      ',name))))))))
