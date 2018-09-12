(cl:in-package :cl-bodge.engine)

;;;
;;;
;;;
(declaim (special *handle-value*))


(defclass handle ()
  ((value :initarg :value :initform (error ":value initarg missing") :reader value-of)))


(defgeneric destroy-handle (handle))
(defgeneric handle-of (object))


(defmacro defhandle (name &key (initform nil)
                            (closeform (error ":closeform must be supplied")))
  "Define foreign object handle that keeps track of foreign instance. :closeform has access to
*handle-value* which contains foreign instance that must be disposed. Foreign instance can be
initialized and returned by :initform or provided to the generated handle constructor ('make- +
`name`)."
  (with-gensyms (handle value)
    `(progn
       (defclass ,name (handle) ())

       (defmethod destroy-handle ((,handle ,name))
         (let ((*handle-value* (value-of ,handle)))
           ,closeform))

       (definline ,(symbolicate 'make- name) (&optional ,value)
         (make-instance ',name :value (or ,value ,initform
                                          (error "value or :initform must be provided")))))))


(defclass foreign-object (disposable)
  ((handle :initarg :handle :initform (error "foreign object :handle must be supplied")
           :reader handle-of))
  (:documentation "Base class for disposable foreign objects. Simplifies
handling of init/dispose lifecycle for such ojects."))


(define-destructor foreign-object ((handle handle-of))
  (destroy-handle handle))


;;
(defclass system-foreign-object (disposable system-object)
  ((handle :initarg :handle :initform (error "foreign object :handle must be supplied")
           :reader handle-of))
  (:documentation "Base class for disposable system-dependent foreign objects. Simplifies
handling of init/dispose lifecycle for such ojects."))


(define-destructor system-foreign-object ((handle handle-of) (sys system-of))
  (run (ge.ng:-> sys :priority :low :important-p t ()
           (destroy-handle handle))))


(definline handle-value-of (object)
  "Return value stored in the handle of the provided foreign object"
  (value-of (handle-of object)))
