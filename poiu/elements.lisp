(in-package :cl-bodge.poiu)


(defgeneric compose (element))


(defun compose-poiu (element context)
  (with-poiu (context)
    (compose element)))


(defclass layout (parent) ())


(defmethod compose ((this layout))
  (dochildren (element this)
    (compose element)))


(defmacro window ((&rest win-opts) &body elements)
  (labels ((expand-element (descriptor)
             (destructuring-bind (name &rest params) (ensure-list descriptor)
               `(,(symbolicate 'make- name) ,@params)))
           (expand-element-hierarchy (root)
             (with-gensyms (parent)
               `(let ((,parent ,(expand-element (car root))))
                  ,@(loop for child in (cdr root)
                       collect `(adopt ,parent ,(expand-element-hierarchy child)))
                  ,parent))))
    (expand-element-hierarchy `((window ,@win-opts) ,@elements))))



;;;
;;;
;;;
(defclass window (layout disposable)
  ((x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (rect :initform (calloc '(:struct (%nk:rect))))
   (width :initarg :width)
   (height :initarg :height)
   (title :initarg :title :initform "")
   (option-mask :initarg :option-mask :initform '())))


(define-destructor window (rect)
  (free rect))


(defun make-window (x y w h &optional (title "") &rest options)
  (make-instance 'window
                 :x x :y y :width w :height h
                 :title title :option-mask (apply #'nk:panel-mask options)))


(defmethod compose ((this window))
  (with-slots (x y width height title option-mask rect) this
    (progn
      (%nk:begin *handle* title (%nk:rect rect x y width height) option-mask)
      (call-next-method)
      (%nk:end *handle*))))


;;;
;;;
;;;
(defclass menu-bar (layout) ())


(defun make-menu-bar ()
  (make-instance 'menu-bar))


(defmethod compose ((this menu-bar))
  (%nk:menubar-begin *handle*)
  (call-next-method)
  (%nk:menubar-end *handle*))


;;;
;;;
;;;
(defclass static-row (layout)
  ((height :initarg :height :initform (error ":height missing"))
   (item-width :initarg :item-width)))


(defun make-static-row-layout (height item-width)
  (make-instance 'static-row
                 :height height
                 :item-width item-width))


(defmethod compose ((this static-row))
  (with-slots (height item-width columns) this
    (%nk:layout-row-static *handle* height (floor item-width) (length (children-of this)))
    (call-next-method)))


(defclass widget () ())


(defclass label-button (widget)
  ((label :initarg :label :initform (error ":label missing"))))


(defun make-label-button (text)
  (make-instance 'label-button :label text))


(defmethod compose ((this label-button))
  (with-slots (label) this
    (%nk:button-label *handle* label)))
