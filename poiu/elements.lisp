(in-package :cl-bodge.poiu)


(defgeneric compose (element))


(defun compose-poiu (element context)
  (with-poiu (context)
    (drain-compose-task-queue context)
    (compose element)))


(defclass layout (parent) ())


(defmethod compose ((this layout))
  (dochildren (element this)
    (compose element)))


(defmacro adopt-layout-by ((&optional parent-layout) &body elements)
  (labels ((expand-element (descriptor)
             (destructuring-bind (name &rest params) (ensure-list descriptor)
               `(,(symbolicate 'make- name) ,@params)))
           (expand-element-hierarchy (root)
             (with-gensyms (parent)
               (let ((children (cdr root))
                     (element (expand-element (car root))))
                 (if children
                     `(let ((,parent ,element))
                        ,@(loop for child in children
                             collect `(adopt ,parent ,(expand-element-hierarchy child)))
                        ,parent)
                     element)))))
    (with-gensyms (p)
      `(let ((,p ,(or parent-layout '(make-instance 'layout))))
         ,@(loop for element in (mapcar #'expand-element-hierarchy elements)
              collect `(adopt ,p ,element))
         ,p))))


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
                                         ,ctx (,ctx :style ,@path &) ,val))
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


;;;
;;;
;;;
(defclass window (layout disposable)
  ((id :initform (symbol-name (gensym "poiu-window")))
   (poiu :initarg :poiu :initform (error ":poiu missing"))
   (x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (panel-p :initarg :panel-p)
   (width :initform nil)
   (height :initform nil)
   (background :initarg :background-color)
   (title :initarg :title :initform "")
   (closed-p :initform nil)
   (option-mask :initarg :option-mask :initform '())
   (nk-rect :initform (calloc '(:struct (%nk:rect))))
   (nk-vec2 :initform (calloc '(:struct (%nk:vec2))))
   (nk-color :initform (calloc '(:struct (%nk:color))))
   (nk-style-item :initform (calloc '(:struct (%nk:style-item))))))


(defun hide-window (window)
  (with-slots (id) window
    (%nk:window-show *handle* id %nk:+hidden+)))


(defun show-window (window)
  (with-slots (id closed-p) window
    (when closed-p
      (setf closed-p nil))
    (%nk:window-show *handle* id %nk:+shown+)))


(defmethod initialize-instance :after ((this window) &key width height hidden-p)
  (with-slots ((w width) (h height) title poiu) this
    (setf w (float width 0f0)
          h (float height 0f0))
    (when hidden-p
      (when-composing (poiu)
        (hide-window this)))))


(define-destructor window (nk-rect nk-color nk-style-item nk-vec2 poiu id)
  (when-composing (poiu)
    (%nk:window-close (handle-value-of poiu) id))
  (free nk-style-item)
  (free nk-vec2)
  (free nk-color)
  (free nk-rect))


(defun make-window (poiu x y w h &key (title "") (background-color nil)
                                   (headerless-p t) (scrollable-p nil) (backgrounded-p nil)
                                   (borderless-p nil) (panel-p nil) (resizable-p nil)
                                   (minimizable-p nil) (movable-p nil) (closable-p nil)
                                   (hidden-p nil))
  (macrolet ((opt (key option)
               `(when ,key
                  (list ,option))))
    (make-instance 'window
                   :poiu poiu
                   :x (f x) :y (f y) :width w :height h
                   :panel-p panel-p
                   :title title
                   :background-color background-color
                   :hidden-p hidden-p
                   :option-mask (apply #'nk:panel-mask
                                       (nconc (opt (not headerless-p) :title)
                                              (opt (not scrollable-p) :no-scrollbar)
                                              (opt (not (or panel-p borderless-p)) :border)
                                              (opt closable-p :closable)
                                              (opt backgrounded-p :background)
                                              (opt resizable-p :scalable)
                                              (opt minimizable-p :minimizable)
                                              (opt movable-p :movable))))))


(defun style-item-color (style-item-buf color-buf color)
  (%nk:style-item-color style-item-buf
                        (%nk:rgba-f color-buf
                                    (x color) (y color)
                                    (z color) (w color))))

(defun compose-window (win next-method)
  (with-slots (x y width height title option-mask nk-rect id) win
    (let ((val (%nk:begin-titled *handle* id title (%nk:rect nk-rect x y width height)
                                 option-mask)))
      (unless (= 0 val)
        (funcall next-method win))
      (%nk:end *handle*))))


(defun compose-panel (win next-method)
  (with-slots (nk-vec2) win
    (with-styles ((%nk:vec2 nk-vec2 :window :spacing)
                  (%nk:vec2 nk-vec2 :window :padding)
                  (%nk:vec2 nk-vec2 :window :header :label-padding)
                  (%nk:vec2 nk-vec2 :window :header :padding)
                  (:float 0.0 :window :border))
      (compose-window win next-method))))


(defmethod compose ((this window))
  (with-slots (background nk-color nk-style-item panel-p closed-p id) this
    (unless closed-p
      (with-styles ((%nk:style-item (when background
                                      (style-item-color nk-style-item nk-color background))
                                    :window :fixed-background))
        (if panel-p
            (compose-panel this #'call-next-method)
            (compose-window this #'call-next-method)))
      (unless (= 0 (%nk:window-is-closed *handle* id))
        (setf closed-p t)))))

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
    (%nk:layout-row-dynamic *handle* (float height 0f0) (length (children-of this)))
    (call-next-method)))


;;;
;;;
;;;
(defclass widget ()
  ((name :initarg :name :initform nil :reader name-of)))


;;;
;;;
;;;
(defclass label-button (widget)
  ((label :initarg :label :initform (error ":label missing"))))


(defun make-label-button (text &key name)
  (make-instance 'label-button :label text :name name))


(defmethod compose ((this label-button))
  (with-slots (label) this
    (unless (= (%nk:button-label *handle* label) 0)
      ;; fixme: propagate event system here somehow
      (post (make-button-click-event this) (events)))))


;;;
;;;
;;;
(defclass label (widget)
  ((text :initarg :text :initform (error ":text missing"))
   (align :initarg :align :initform (error ":align missing"))))


(defun text-align->nk (align)
  (ecase align
    (:left %nk:+text-align-left+)
    (:right %nk:+text-align-right+)))


(defun make-text-label (text-or-supplier &key name (align :left))
  (make-instance 'label
                 :text text-or-supplier
                 :name name
                 :align (text-align->nk align)))


(defmethod compose ((this label))
  (with-slots (text align) this
    (let ((text (if (functionp text)
                    (stringify (funcall text))
                    text)))
      (%nk:label *handle* text align))))


;;;
;;;
;;;
(defclass text-edit (disposable widget)
  ((buffer :initform (calloc '(:struct (%nk:text-edit))))))


(defmethod initialize-instance :after ((this text-edit) &key)
  (with-slots (buffer) this
    (%nk:textedit-init-default buffer)))


(define-destructor text-edit (buffer)
  (free buffer))


(defun make-text-edit (&key name)
  (make-instance 'text-edit :name name))


(defmethod compose ((this text-edit))
  (with-slots (buffer) this
    (%nk:edit-buffer *handle* %nk:+edit-simple+ buffer
                     (ge.util:foreign-function-pointer '%nk:filter-default))))


;;;
;;;
;;;
(defclass health-monitor ()
  ((window :initform nil)))


(defmethod initialize-instance :after ((this health-monitor) &key poiu x y width height hidden-p)
  (with-slots (window) this
    (setf window (make-window poiu x y width height :title "Health monitor"
                              :headerless-p nil :scrollable-p t :resizable-p t
                              :movable-p t :closable-p t :hidden-p hidden-p))))


(defun make-health-monitor (poiu x y &key (width 640.0) (height 480.0) (hidden-p t))
  (make-instance 'health-monitor :poiu poiu
                 :x x :y y :width width :height height :hidden-p hidden-p))


(defun show-health-monitor (mon)
  (with-slots (window) mon
    (show-window window)))


(defun hide-health-monitor (mon)
  (with-slots (window) mon
    (hide-window window)))


(defun add-simple-reporter (win label supplier)
  (with-slots (window) win
    (let ((row (make-dynamic-row-layout 24)))
      (adopt row (make-text-label label))
      (adopt row (make-text-label supplier :align :right))
      (adopt window row))))


(defmethod compose ((this health-monitor))
  (with-slots (window) this
    (compose window)))


;;;
;;;
;;;
(defclass spacing (layout) ())


(defun make-spacing ()
  (make-instance 'spacing))


(defmethod compose ((this spacing))
  (%nk:spacing *handle* 1))
