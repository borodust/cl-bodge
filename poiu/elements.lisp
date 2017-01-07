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
   (panel-mode-p :initarg :panel-mode-p)
   (width :initarg :width)
   (height :initarg :height)
   (background :initarg :background-color)
   (title :initarg :title :initform "")
   (option-mask :initarg :option-mask :initform '())

   (nk-rect :initform (calloc '(:struct (%nk:rect))))
   (nk-vec2 :initform (calloc '(:struct (%nk:vec2))))
   (nk-color :initform (calloc '(:struct (%nk:color))))
   (nk-style-item :initform (calloc '(:struct (%nk:style-item))))))


(define-destructor window (nk-rect nk-color nk-style-item nk-vec2)
  (free nk-style-item)
  (free nk-vec2)
  (free nk-color)
  (free nk-rect))


(defun make-window (x y w h &key (title "") (background-color nil)
                              (headerless-p t) (scrollable-p nil)
                              (borderless-p t) (panel-mode-p nil) (resizable-p nil)
                              (minimizable-p nil) (movable-p nil))
  (macrolet ((opt (key option)
               `(when ,key
                  (list ,option))))
    (make-instance 'window
                 :x x :y y :width w :height h
                 :panel-mode-p panel-mode-p
                 :title title
                 :background-color background-color
                 :option-mask (apply #'nk:panel-mask
                                     (nconc (opt (not headerless-p) :title)
                                            (opt (not scrollable-p) :no-scrollbar)
                                            (opt (not borderless-p) :border)
                                            (opt resizable-p :scalable)
                                            (opt minimizable-p :minimizable)
                                            (opt movable-p :movable))))))


(defun style-item-color (style-item-buf color-buf color)
  (%nk:style-item-color style-item-buf
                        (%nk:rgba-f color-buf
                                    (x color) (y color)
                                    (z color) (w color))))


;; todo: wrap each push/pop into proper unwind-protect?
(defmacro with-styles ((&rest styles) &body body)
  (with-gensyms (ctx)
    (let ((stack-ops (loop with nk-package = (find-package :%nk)
                        for style in styles
                        collect
                          (destructuring-bind (type new-value &rest path) style
                            (with-gensyms (val)
                              (list val
                                    new-value
                                    `(when ,val
                                       (,(format-symbol nk-package "~A~A" 'style-push- type)
                                         ,ctx (,ctx :style ,@path) ,val))
                                    `(when ,val
                                       (,(format-symbol nk-package "~A~A"'style-pop- type)
                                         ,ctx))))))))
      `(c-let ((,ctx (:struct (%nk:context)) :from *handle*))
         (let ,(mapcar (lambda (l) (list (first l) (second l))) stack-ops)
           (unwind-protect
                (progn
                  ,@(mapcar #'third stack-ops)
                  ,@body))
           ,@(reverse (mapcar #'fourth stack-ops)))))))


(defmethod compose ((this window))
  (with-slots (x y width height title option-mask nk-rect background nk-color nk-style-item
                 panel-mode-p)
      this
    (with-styles ((%nk:style-item (when background
                                    (style-item-color nk-style-item nk-color background))
                                  :window :fixed-background))
      (%nk:begin *handle* title (%nk:rect nk-rect x y width height) option-mask)
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
  (with-slots (height item-width) this
    (%nk:layout-row-static *handle* height (floor item-width) (length (children-of this)))
    (call-next-method)))


;;;
;;;
;;;
(defclass dynamic-row (layout)
  ((height :initarg :height :initform (error ":height missing"))))


(defun make-dynamic-row-layout (height)
  (make-instance 'dynamic-row :height height))


(defmethod compose ((this dynamic-row))
  (with-slots (height) this
    (%nk:layout-row-dynamic *handle* height (length (children-of this)))
    (call-next-method)))


;;;
;;;
;;;
(defclass widget () ())


(defclass label-button (widget)
  ((label :initarg :label :initform (error ":label missing"))))


(defun make-label-button (text)
  (make-instance 'label-button :label text))


(defmethod compose ((this label-button))
  (with-slots (label) this
    (%nk:button-label *handle* label)))
