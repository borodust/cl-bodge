(cl:in-package :cl-bodge.ui)


(declaim (special *window*))


(defgeneric compose (element))


(defclass named ()
  ((name :initarg :name :initform nil :reader name-of)))


(defclass layout (named parent) ())


(defmethod compose ((this layout))
  (dochildren (element this)
    (compose element)))


(definline make-container-layout ()
  (make-instance 'layout))


(defmacro with-style (((&rest path) value) &body body)
  (with-gensyms (ctx)
    `(c-let ((,ctx (:struct (%nk:context)) :from (handle-value-of *context*)))
       (push-style *context* (,ctx :style ,@path &) ,value)
       (unwind-protect
            (progn ,@body)
         (pop-style *context*)))))


(defmacro with-styles ((&rest styles) &body body)
  (labels ((expand-next (rest-styles)
             (if-let ((path (first rest-styles))
                      (value (second rest-styles)))
               `(with-style ((,@path) ,value)
                  ,(expand-next (cddr rest-styles)))
               `(progn ,@body))))
    (expand-next styles)))

;;;
;;;
;;;
(defclass window (layout)
  ((id :initform (%next-window-id))
   (x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (panel-p :initarg :panel-p)
   (width :initform nil)
   (height :initform nil)
   (background-style-item :initform nil)
   (title :initarg :title :initform "")
   (hidden-p :initform nil :reader hiddenp)
   (option-mask :initarg :option-mask :initform '())
   (redefined-p :initform nil)))


(defun hide-window (window)
  (with-slots (hidden-p) window
    (unless hidden-p
      (setf hidden-p t))))


(defun show-window (window)
  (with-slots (hidden-p) window
    (when hidden-p
      (setf hidden-p nil))))


(defmethod initialize-instance :after ((this window)
                                       &key
                                         (width (error ":width missing"))
                                         (height (error ":height missing"))
                                         (origin (vec2 0 0))
                                         (title "") (background-color nil)
                                         (panel-p nil) (hidden nil))
  (with-slots ((w width) (h height) x y background-style-item option-mask
               (this-panel-p panel-p) (this-title title))
      this
    (setf w (float width 0f0)
          h (float height 0f0)
          x (x origin)
          y (y origin)
          this-panel-p panel-p
          this-title title)
    (when background-color
      (setf background-style-item (make-instance 'color-style-item :color background-color)))
    (when hidden
      (hide-window this))
    (update-window-layout this)))


(defun add-window (window-class ui &rest initargs &key &allow-other-keys)
  (with-ui-access (ui)
    (push (apply #'make-instance window-class initargs) (%windows-of ui))))


(defun find-element (name &optional (window *window*))
  (labels ((%find-element (root name)
             (if (equal (name-of root) name)
                 root
                 (loop for child in (children-of root)
                    thereis (%find-element child name)))))
    (%find-element window name)))


(defun compose-window (win next-method)
  (with-slots (x y width height title option-mask id) win
    (c-with ((bounds (:struct (%nk:rect))))
      (setf (bounds :x) x
            (bounds :y) (f  (- (height-of *context*) y height))
            (bounds :w) width
            (bounds :h) height)
      (let ((val (%nk:begin-titled *handle* id title bounds option-mask)))
        (unless (= 0 val)
          (funcall next-method win))
        (%nk:end *handle*)))))


(defun compose-panel (win next-method)
  (let ((vec (vec2 0.0 0.0)))
    (with-styles ((:window :spacing) vec
                  (:window :padding) vec
                  (:window :header :label-padding) vec
                  (:window :header :padding) vec
                  (:window :border) 0.0)
      (compose-window win next-method))))


(defmethod compose ((this window))
  (with-slots (background-style-item panel-p hidden-p id redefined-p layout-constructor)
      this
    (unless hidden-p
      (when redefined-p
        (update-window-layout this)
        (setf redefined-p nil))
      (with-styles ((:window :fixed-background) background-style-item)
        (let ((*window* this))
          (if panel-p
              (compose-panel this #'call-next-method)
              (compose-window this #'call-next-method))))
      (unless (= 0 (%nk:window-is-closed *handle* id))
        (setf hidden-p t)))))


(defmacro layout ((parent-layout) &body elements)
  (labels ((expand-element (root)
             (when (atom root)
               (error "Element descriptor must be a list, but got ~A" root))
             (with-gensyms (parent)
               (destructuring-bind (element-class &rest initargs-and-children) root
                 (multiple-value-bind (initargs children)
                     (ge.util:parse-initargs-and-list initargs-and-children)
                   `(let ((,parent (make-instance ',element-class ,@initargs)))
                      ,@(loop for child in children
                              collect `(adopt ,parent ,(expand-element child)))
                      ,parent))))))
    (once-only (parent-layout)
      `(prog1 ,parent-layout
         ,@(loop for element in (mapcar #'expand-element elements)
                 collect `(adopt ,parent-layout ,element))))))


(defmethod update-instance-for-redefined-class :after ((this window)
                                                       added-slots
                                                       discarded-slots
                                                       property-list
                                                       &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (with-slots (redefined-p) this
    (setf redefined-p t)))


(defgeneric update-window-layout (window)
  (:method (window) (declare (ignore window))))


(defun update-window-options (window &rest opts)
  (with-slots (option-mask) window
    (flet ((to-nuklear-opts (opts)
             (let ((updated-opts (list :title :no-scrollbar :border)))
               (loop for opt in opts
                     do (case opt
                          (:resizable (push :scalable updated-opts))
                          (:headerless (deletef updated-opts :title))
                          (:borderless (deletef updated-opts :border))
                          (:closable (push :closable updated-opts))
                          (:minimizable (push :minimizable updated-opts))
                          (:movable (push :movable updated-opts))
                          (:backgrounded (push :background updated-opts))
                          (:scrollable (deletef updated-opts :no-scrollbar))))
               updated-opts)))
      (setf option-mask (apply #'nk:panel-mask (to-nuklear-opts opts))))))


(defmacro defwindow (name-and-opts &body layout)
  (flet ((filter-window-initargs (opts)
           (loop with special-keywords = '(:inherit :options)
                 for (key value) in opts
                 unless (member key special-keywords)
                   append (list key value))) )
    (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
      (with-gensyms (layout-parent)
        `(progn
           (defclass ,name (window ,@(assoc-value opts :inherit)) ()
             (:default-initargs ,@(filter-window-initargs opts)))
           (defmethod update-window-layout ((,layout-parent ,name))
             (update-window-options ,layout-parent ,@(assoc-value opts :options))
             (abandon-all ,layout-parent)
             (layout (,layout-parent) ,@layout))
           (make-instances-obsolete ',name))))))

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
(defclass vertical-layout (layout)
  ((row-height :initarg :row-height :initform (error ":row-height missing"))))


(defmethod compose ((this vertical-layout))
  (with-slots (row-height) this
    (%nk:layout-row-dynamic *handle* (f row-height) 1)
    (call-next-method)))
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
(defclass button (widget)
  ((label :initarg :label :initform "")
   (click-listener :initarg :on-click :initform nil)))


(defmethod compose ((this button))
  (with-slots (label click-listener) this
    (unless (or (= (%nk:button-label *handle* label) 0) (null click-listener))
      (funcall click-listener *window* (make-button-click-event this)))))


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
          (let ((ptr (%nk:str-get-const str-info)))
            (or (cffi:foreign-string-to-lisp ptr
                                             :count len
                                             :encoding :utf-8)
                "")))))))


(defmethod compose ((this text-edit))
  (with-slots (buffer) this
    (%nk:edit-buffer *handle* %nk:+edit-simple+ buffer
                     (claw:foreign-function-pointer '%nk:filter-default))))


;;
(defgeneric item-status (item))
(defgeneric item-name-of (item))
(defgeneric item-selected-p (item))
(defgeneric select-item (item status))


(defclass list-select-text-item (disposable)
  ((text :initarg :text :reader item-name-of)
   (status-buf :initform (calloc :int) :reader item-status)))


(define-destructor list-select-text-item (status-buf)
  (free status-buf))


(defmethod item-selected-p ((this list-select-text-item))
  (/= 0 (c-ref (item-status this) :int )))


(defmethod select-item ((this list-select-text-item) status)
  (setf (c-ref (item-status this) :int) (if status 1 0)))


(defgeneric add-item (object item))
(defgeneric clear (object))

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
        ;; todo: invoke listeners
        (dolist (other-item items)
          (unless (eq item other-item)
            (select-item other-item nil)))))))

;;;
;;;
;;;
(defclass debug-console (window) ())


(defun show-debug-console (mon)
  (with-slots (window) mon
    (show-window window)))


(defun hide-debug-console (mon)
  (with-slots (window) mon
    (hide-window window)))


(defun add-simple-reporter (win label value-supplier)
  (with-slots (window) win
    (let ((row (make-dynamic-row-layout 24)))
      (adopt row (make-text-label label))
      (adopt row (make-text-label value-supplier :align :right))
      (adopt window row))))


(defmethod compose ((this debug-console))
  (with-slots (window) this
    (compose window)))
