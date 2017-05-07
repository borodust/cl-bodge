(in-package :cl-bodge.poiu)


(defgeneric compose (element))


(defun compose-poiu (element context)
  (with-poiu (context)
    (drain-compose-task-queue context)
    (compose element)))


(defclass named ()
  ((name :initarg :name :initform nil :reader name-of)))


(defclass layout (named parent) ())


(defmethod compose ((this layout))
  (dochildren (element this)
    (compose element)))


(definline make-container-layout ()
  (make-instance 'layout))


(defmacro adopt-layout-by ((parent-layout) &body elements)
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
    (once-only (parent-layout)
      `(prog1 ,parent-layout
         ,@(loop for element in (mapcar #'expand-element-hierarchy elements)
              collect `(adopt ,parent-layout ,element))))))


;; todo: wrap each push/pop into proper unwind-protect?
(defmacro with-styles ((&rest styles) &body body)
  (with-gensyms (ctx)
    (let ((stack-ops (loop for style in styles
                        collect
                          (destructuring-bind (new-value &rest path) style
                            (with-gensyms (val)
                              (list val
                                    new-value
                                    `(when ,val
                                       (push-style *context* (,ctx :style ,@path &) ,val))
                                    `(when ,val
                                       (if (subtypep (class-of ,val) 'style-item)
                                           (pop-style *context* 'style-item)
                                           (pop-style *context* (class-name-of ,val))))))))))
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
   (background-style-item :initform nil)
   (title :initarg :title :initform "")
   (hidden-p :initform nil :reader hiddenp)
   (option-mask :initarg :option-mask :initform '())))


(defun hide-window (window)
  (with-slots (hidden-p) window
    (unless hidden-p
      (setf hidden-p t))))


(defun show-window (window)
  (with-slots (hidden-p) window
    (when hidden-p
      (setf hidden-p nil))))


(defmethod initialize-instance :after ((this window) &key width height hidden-p background-color)
  (with-slots ((w width) (h height) title poiu background-style-item) this
    (setf w (float width 0f0)
          h (float height 0f0))
    (when background-color
      (setf background-style-item (make-instance 'color-style-item :color background-color)))
    (when hidden-p
      (hide-window this))))


(define-destructor window (poiu id)
  (when-composing (poiu)
    (%nk:window-close (handle-value-of poiu) id)))


(defun make-window (poiu origin w h &key (title "") (background-color nil)
                                      (headerless t) (scrollable nil) (background-p nil)
                                      (borderless nil) (panel-p nil) (resizable nil)
                                      (minimizable nil) (movable nil) (closable nil)
                                      (hidden nil))
  (macrolet ((opt (key option)
               `(when ,key
                  (list ,option))))
    (make-instance 'window
                   :poiu poiu
                   :x (x origin) :y (y origin) :width w :height h
                   :panel-p panel-p
                   :title title
                   :background-color background-color
                   :hidden-p hidden
                   :option-mask (apply #'nk:panel-mask
                                       (nconc (opt (not headerless) :title)
                                              (opt (not scrollable) :no-scrollbar)
                                              (opt (not (or panel-p borderless)) :border)
                                              (opt closable :closable)
                                              (opt background-p :background)
                                              (opt resizable :scalable)
                                              (opt minimizable :minimizable)
                                              (opt movable :movable))))))


(defun find-element (window name)
  (labels ((%find-element (root name)
             (if (equal (name-of root) name)
                 root
                 (loop for child in (children-of root)
                    thereis (%find-element child name)))))
    (%find-element window name)))


(defun style-item-color (style-item color)
  (%nk:bge-init-color-style-item style-item (x color) (y color) (z color) (w color)))


(defun compose-window (win next-method)
  (with-slots (x y width height title option-mask id) win
    (let ((val (%nk:bge-begin-titled *handle* id title
                                     x (f  (- (height-of *context*) y height))
                                     width height option-mask)))
      (unless (= 0 val)
        (funcall next-method win))
      (%nk:end *handle*))))


(defun compose-panel (win next-method)
  (let ((vec (vec2 0.0 0.0)))
    (with-styles ((vec :window :spacing)
                  (vec :window :padding)
                  (vec :window :header :label-padding)
                  (vec :window :header :padding)
                  (0.0 :window :border))
      (compose-window win next-method))))


(defmethod compose ((this window))
  (with-slots (background-style-item panel-p hidden-p id) this
    (unless hidden-p
      (with-styles ((background-style-item :window :fixed-background))
        (if panel-p
            (compose-panel this #'call-next-method)
            (compose-window this #'call-next-method)))
      (unless (= 0 (%nk:window-is-closed *handle* id))
        (setf hidden-p t)))))


(defmacro defwindow (name-and-opts &body layout)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (with-gensyms (poiu origin width height)
      `(defun ,(symbolicate 'make- name '-window) (,poiu ,origin ,width ,height)
         (adopt-layout-by ((apply #'make-window ,poiu
                                  (x ,origin) (y ,origin)
                                  ,width ,height
                                  ,@opts))
           ,@layout)))))
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
    (%nk:layout-row-static *handle* (f height) (floor item-width) (length (children-of this)))
    (call-next-method)))


;;;
;;;
;;;
(defclass dynamic-row (layout)
  ((height :initarg :height :initform (error ":height missing"))
   (columns :initform nil :initarg :columns)))


(defun make-dynamic-row-layout (height &key columns name)
  (make-instance 'dynamic-row :height height :columns columns :name name))


(defmethod compose ((this dynamic-row))
  (with-slots (height columns) this
    (%nk:layout-row-dynamic *handle* (f height) (or (and columns (floor columns))
                                                    (length (children-of this))))
    (call-next-method)))


;;;
;;;
;;;
(defgeneric hide-widget (widget))
(defgeneric show-widget (widget))


(defclass widget (named)
  ((hidden :initform nil :reader hiddenp)))


(defmethod children-of ((this widget))
  nil)


(defmethod hide-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden t)))


(defmethod show-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden nil)))


(defmethod compose :around ((this widget))
  (unless (hiddenp this)
    (call-next-method)))

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
      (post 'button-click-event :poiu-button this))))


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


;;
(defclass spacing (widget)
  ((columns :initform 1 :initarg :columns)))


(defun make-spacing (&optional (columns 1))
  (make-instance 'spacing :columns columns))


(defmethod compose ((this spacing))
  (with-slots (columns) this
    (%nk:spacing *handle* (floor columns))))


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


(defgeneric text-of (object)
  (:method ((this text-edit))
    (with-slots (buffer) this
      (c-let ((buf (:struct (%nk:text-edit)) :from buffer))
        (let* ((str-info (buf :string))
               (len (%nk:str-len-char str-info)))
          (inhibit-string-conversion
            (multiple-value-bind (text ptr) (%nk:str-get-const str-info)
              (declare (ignore text))
              (cffi:foreign-string-to-lisp ptr
                                           :count len
                                           :encoding :utf-8))))))))


(defmethod compose ((this text-edit))
  (with-slots (buffer) this
    (%nk:edit-buffer *handle* %nk:+edit-simple+ buffer
                     (ge.util:foreign-function-pointer '%nk:filter-default))))


;;
(defgeneric item-status (item))
(defgeneric item-name-of (item))
(defgeneric item-selected-p (item))
(defgeneric select-item (item status))


(defclass list-select-text-item (disposable)
  ((text :initarg :text :reader item-name-of)
   (status-buf :initform (calloc-ptr :int) :reader item-status)))


(define-destructor list-select-text-item (status-buf)
  (free status-buf))


(defmethod item-selected-p ((this list-select-text-item))
  (/= 0 (c-ref (item-status this) :int )))


(defmethod select-item ((this list-select-text-item) status)
  (setf (c-ref (item-status this) :int) (if status 1 0)))


(defgeneric add-item (object item))
(defgeneric clear (ovject))
(defclass list-select (widget)
  ((items :initform nil)
   (item-height :initarg :item-height)))


(defun make-list-select (item-height &key name)
  (make-instance 'list-select :item-height item-height :name name))


(defmethod add-item ((this list-select) (text string))
  (with-slots (items) this
    (nconcf items (list (make-instance 'list-select-text-item :text text)))))


(defmethod clear ((this list-select))
  (with-slots (items) this
    (setf items nil)))


(defmethod compose ((this list-select))
  (with-slots (items item-height status-buf) this
    (%nk:layout-row-dynamic *handle* (float item-height) 1)
    (dolist (item items)
      (unless (= 0 (%nk:selectable-label *handle*
                                         (item-name-of item)
                                         %nk:+text-left+
                                         (item-status item)))
        (post 'item-selected :source this :item item)
        (dolist (other-item items)
          (unless (eq item other-item)
            (select-item other-item nil)))))))

;;;
;;;
;;;
(defclass health-monitor ()
  ((window :initform nil)))


(defmethod initialize-instance :after ((this health-monitor) &key poiu origin width height hidden)
  (with-slots (window) this
    (setf window (make-window poiu origin width height :title "Health monitor"
                              :headerless nil :scrollable t :resizable t
                              :movable t :closable t :hidden hidden))))


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
