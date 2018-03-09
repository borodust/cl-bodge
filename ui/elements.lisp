(cl:in-package :cl-bodge.ui)


(declaim (special *window*
                  *layout*))


(defgeneric compose (element))

(defvar *zero-vec2* (vec2))
(defvar *one-vec2* (vec2 1 1))
(defvar *nk-buttons* (list %nk:+button-left+ :left
                           %nk:+button-right+ :right
                           %nk:+button-middle+ :middle))



(defclass named ()
  ((name :initarg :name :initform nil :reader name-of)))


(defun max-reducer (property-supplier)
  (lambda (value element)
    (if (not value)
        (funcall property-supplier element)
        (when-let ((element-value (funcall property-supplier element)))
          (max value element-value)))))


(defgeneric calc-bounds (element))


(defclass layout (named parent) ())


(defmethod compose ((this layout))
  (let ((*layout* this))
    (dochildren (element this)
      (compose element))))


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
(defclass basic-panel ()
  ((panel-id :initform (%next-panel-id) :reader %panel-id-of)))


;;;
;;;
;;;
(defclass window (basic-panel)
  ((x :initarg :x :initform 0.0)
   (y :initarg :y :initform 0.0)
   (width :initform nil)
   (height :initform nil)
   (background-style-item :initform nil)
   (title :initarg :title :initform "")
   (hidden-p :initform nil :reader hiddenp)
   (option-mask :initarg :option-mask :initform '())
   (style :initform (make-default-style))
   (layout :initform (make-instance 'vertical-layout))
   (redefined-p :initform nil)))


(defun hide-window (window)
  (with-slots (hidden-p) window
    (unless hidden-p
      (setf hidden-p t))))


(defun show-window (window)
  (with-slots (hidden-p) window
    (when hidden-p
      (setf hidden-p nil))))


(defun setup-window (window &key
                              (width (error ":width missing"))
                              (height (error ":height missing"))
                              (origin (vec2 0 0))
                              (title "") (background-color nil)
                              (hidden nil)
                     &allow-other-keys)
  (with-slots ((w width) (h height) x y background-style-item
               option-mask (this-title title))
      window
    (setf w (float width 0f0)
          h (float height 0f0)
          x (x origin)
          y (y origin)
          this-title title)
    (when background-color
      (setf background-style-item (make-instance 'color-style-item :color background-color)))
    (when hidden
      (hide-window window))))


(defmethod initialize-instance :after ((this window) &key &allow-other-keys)
  (reinitialize-window this))


(defmethod children-of ((this window))
  (with-slots (layout) this
    (children-of layout)))


(defmethod adopt ((this window) child)
  (with-slots (layout) this
    (adopt layout child)))


(defmethod abandon ((this window) child)
  (with-slots (layout) this
    (abandon layout child)))


(defmethod abandon-all ((this window))
  (with-slots (layout) this
    (abandon-all layout)))


(defmethod compose ((this window))
  (with-slots (layout) this
    (compose layout)))


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


(defun compose-window (win)
  (with-slots (x y width height title option-mask layout) win
    (c-with ((bounds (:struct (%nk:rect))))
      (setf (bounds :x) x
            (bounds :y) (f (- (height-of *context*) y height))
            (bounds :w) width
            (bounds :h) height)
      (let ((val (%nk:begin-titled *handle* (%panel-id-of win) title bounds option-mask)))
        (unwind-protect
             (unless (= 0 val)
               (with-styles ((:window :spacing) *zero-vec2*
                             (:window :padding) *zero-vec2*
                             (:window :group-padding) *one-vec2*)
                 (compose layout)))
          (%nk:end *handle*))))))


(defmethod compose ((this window))
  (with-slots (background-style-item hidden-p redefined-p style)
      this
    (unless hidden-p
      (when redefined-p
        (reinitialize-window this)
        (setf redefined-p nil))
      (with-styles ((:window :fixed-background) background-style-item)
        (let ((*window* this)
              (*style* style)
              (*layout* nil))
          (compose-window this))))
      (unless (= 0 (%nk:window-is-closed *handle* (%panel-id-of this)))
        (setf hidden-p t))))


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


(defgeneric reinitialize-window (window)
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
                 for (key . value) in opts
                 unless (member key special-keywords)
                   append (case key
                            (:origin (list key `(vec2 ,(or (first value) 0) ,(or (second value) 0))))
                            (t (list key (first value)))))))
    (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
      (with-gensyms (layout-parent)
        (let ((initargs (filter-window-initargs opts)))
          `(progn
             (defclass ,name (window ,@(assoc-value opts :inherit)) ()
               (:default-initargs ,@initargs))
             (defmethod reinitialize-window ((,layout-parent ,name))
               (setup-window ,layout-parent ,@initargs)
               (update-window-options ,layout-parent ,@(assoc-value opts :options))
               (abandon-all ,layout-parent)
               (layout (,layout-parent) ,@layout))
             (make-instances-obsolete ',name)))))))

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
(defclass panel (basic-panel)
  ((content :initarg :content :initform (error ":content missing"))))


(defmethod calc-bounds ((this panel))
  (with-slots (content) this
    (calc-bounds content)))


(defmethod compose ((this panel))
  (with-slots (content) this
    (let ((begin-result (%nk:group-begin-titled *handle*
                                                (%panel-id-of this)
                                                ""
                                                (nk:panel-mask :no-scrollbar))))
      (unless (= begin-result 0)
        (unwind-protect
             (let ((*layout* nil))
               (compose content))
          (%nk:group-end *handle*))))))
;;;
;;;
;;;
(defun default-row-height (child-height)
  (f (+ (or child-height (style :row-height)) (style :row-padding))))


(defclass vertical-layout (layout) ())


(defmethod adopt ((this vertical-layout) child)
  (call-next-method this (make-instance 'panel :content child)))


(defmethod calc-bounds ((this vertical-layout))
  (let (width height)
    (dochildren (child this)
      (multiple-value-bind (child-width child-height) (calc-bounds child)
        (when child-width
          (setf width (if width
                          (max width child-width)
                          child-width)))
        (let ((child-height (default-row-height child-height)))
          (setf height (if height
                           (+ height child-height)
                           child-height)))))
    (values width height)))


(defmethod compose ((this vertical-layout))
  (dochildren (child this)
    (multiple-value-bind (child-width child-height) (calc-bounds child)
      (let ((height (default-row-height child-height)))
        (if child-width
            (%nk:layout-row-static *handle* height (floor child-width) 1)
            (%nk:layout-row-dynamic *handle* height 1)))
      (compose child))))

;;;
;;;
;;;
(defclass horizontal-layout (layout) ())


(defmethod adopt ((this horizontal-layout) child)
  (call-next-method this (make-instance 'panel :content child)))


(defmethod calc-bounds ((this horizontal-layout))
  (let (width height)
    (dochildren (child this)
      (multiple-value-bind (child-width child-height) (calc-bounds child)
        (when child-width
          (setf width (if width
                          (+ width child-width)
                          child-width)))
        (when child-height
          (setf height (if height
                          (max height child-height)
                          child-height)))))
    (values width height)))


(defmethod compose ((this horizontal-layout))
  (flet ((height-max (value element)
           (multiple-value-bind (el-width el-height) (calc-bounds element)
             (declare (ignore el-width))
             (if value
                 (when el-height
                   (max el-height value))
                 el-height))))
    (let ((height (f (or (reduce #'height-max (children-of this) :initial-value nil)
                         (style :row-height)))))
      (%nk:layout-row-dynamic *handle* height (length (children-of this)))
      (call-next-method))))


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
  ((hidden :initform nil :reader hiddenp)
   (width :initform nil :initarg :width :reader width-of)
   (height :initform nil :initarg :height :reader height-of)))


(defmethod hide-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden t)))


(defmethod show-widget ((this widget))
  (with-slots (hidden) this
    (setf hidden nil)))


(defmethod calc-bounds ((this widget))
  (values (width-of this) (height-of this)))


(defmethod compose :around ((this widget))
  (unless (hiddenp this)
    (when (eq *layout* nil)
      (let ((height (f (or (height-of this) (style :row-height)))))
        (if-let ((width (width-of this)))
          (%nk:layout-row-static *handle* height (floor width) 1)
          (%nk:layout-row-dynamic *handle* height 1))))
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
      (funcall click-listener *window* (make-ui-event this)))))


;;;
;;;
;;;
(defclass option (widget)
  ((label :initarg :label :initform "")
   (enabled-p :initarg :enabled-p :initform nil)
   (click-listener :initarg :on-click :initform nil)))


(defmethod compose ((this option))
  (with-slots (enabled-p click-listener label) this
    (let ((return-value (%nk:option-label *handle* label (if enabled-p 1 0))))
      (unless (or (= return-value %nk:+false+) (null click-listener))
        (funcall click-listener *window* (make-ui-event this))))))


;;;
;;;
;;;
(defun text-align->nk (align)
  (ecase align
    (:left %nk:+text-align-left+)
    (:right %nk:+text-align-right+)
    (:bottom %nk:+text-align-bottom+)
    (:center %nk:+text-align-centered+)
    (:middle %nk:+text-align-middle+)
    (:top %nk:+text-align-top+)))


(defclass label (widget)
  ((text :initarg :text :initform "")
   (align :initarg :align)))


(defmethod initialize-instance :after ((this label) &key (align :left))
  (with-slots ((this-align align)) this
    (setf this-align (text-align->nk align))))


(defmethod compose ((this label))
  (with-slots (text align) this
    (let ((text (if (functionp text)
                    (stringify (funcall text))
                    text)))
      (%nk:label *handle* text align))))

;;;
;;;
;;;
(defclass combo-box (layout widget)
  ((label :initarg :label :initform "")
   (color :initarg :color :initform nil)
   (height :initarg :height :initform 400f0)))


(defmethod compose ((this combo-box))
  (with-slots (height (this-color color) label) this
    (claw:c-with ((size (:struct (%nk:vec2))))
      (setf (size :x) (%nk:widget-width *handle*)
            (size :y) (f height))
      (flet ((combo-begin ()
               (cond
                 (this-color
                  (claw:c-with ((color (:struct (%nk:colorf))))
                    (setf (color :r) (x this-color)
                          (color :g) (y this-color)
                          (color :b) (z this-color)
                          (color :a) (w this-color))
                    (%nk:combo-begin-color *handle* (%nk:rgb-cf color color) size)))
                 (t (%nk:combo-begin-label *handle* label size)))))
        (unless (= (combo-begin) 0)
          (unwind-protect
               (call-next-method)
            (%nk:combo-end *handle*)))))))

;;;
;;;
;;;
(defclass float-property (disposable widget)
  ((label :initarg :label :initform "")
   (min :initarg :start :initform 0f0)
   (max :initarg :end :initform 1f0)
   (step :initarg :step :initform 0f0)
   (increment :initarg :increment :initform 0.005f0)
   (value :initarg :value :initform 0f0)))


(defmethod compose ((this float-property))
  (with-slots (value min max step label increment) this
    (setf value (%nk:propertyf *handle* label
                               (f min) (f value) (f max)
                               (f step) (f increment)))))
;;;
;;;
;;;
(defclass color-picker (widget disposable)
  ((color)))


(defmethod initialize-instance :after ((this color-picker) &key (color (vec4 1.0 1.0 1.0 1.0)))
  (with-slots ((this-color color)) this
    (setf this-color (c-let ((color-f (:struct (%nk:colorf))))
                       (setf (color-f :r) (x color)
                             (color-f :g) (y color)
                             (color-f :b) (z color)
                             (color-f :a) (w color))
                       color-f))))


(define-destructor color-picker (color)
  (free color))


(defmethod compose ((this color-picker))
  (with-slots (color) this
    (%nk:color-picker color *handle* color %nk:+rgba+)))



;;;
;;;
;;;
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
(defgeneric render-custom-widget (widget origin width height))
(defgeneric update-widget (widget)
  (:method (widget) (declare (ignore widget))))


(defclass custom-widget (disposable widget)
  ((id :initform (%next-custom-widget-id) :reader %id-of)
   (bounds)
   (clicked-buttons :initform nil)
   (pressed-buttons :initform nil)
   (hovered-p :initform nil)))


(defmethod initialize-instance :after ((this custom-widget) &key)
  (with-slots (bounds) this
    (setf bounds (calloc '(:struct (%nk:rect))))))


(define-destructor custom-widget (bounds)
  (free bounds))


(defmethod update-widget ((this custom-widget))
  (with-slots (clicked-buttons pressed-buttons hovered-p bounds) this
    (c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (flet ((widget-hovered-p ()
               (= %nk:+true+ (%nk:input-is-mouse-hovering-rect (ctx :input) bounds)))
             (widget-clicked-p (button)
               (= %nk:+true+ (%nk:input-has-mouse-click-in-rect (ctx :input) button bounds)))
             (widget-pressed-p (button)
               (= %nk:+true+ (%nk:input-has-mouse-click-down-in-rect (ctx :input) button bounds
                                                                     %nk:+true+))))
        (setf hovered-p (widget-hovered-p)
              clicked-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                    when (widget-clicked-p nk-key)
                                      collect key)
              pressed-buttons (loop for (nk-key key) on *nk-buttons* by #'cddr
                                    when (widget-pressed-p nk-key)
                                      collect key))))))


(defun custom-widget-hovered-p (widget)
  (with-slots (hovered-p) widget
    hovered-p))


(defun custom-widget-clicked-p (widget button)
  (with-slots (clicked-buttons) widget
    (member button clicked-buttons)))


(defun custom-widget-pressed-p (widget button)
  (with-slots (pressed-buttons) widget
    (member button pressed-buttons)))


(defmethod compose ((this custom-widget))
  (with-slots (bounds) this
    (%nk:widget bounds *handle*)
    (update-widget this)
    (setf (context-custom-widget (%id-of this)) this)
    (c-let ((ctx (:struct (%nk:context)) :from *handle*))
      (%nk:push-custom (ctx :current :buffer)
                       bounds nil (cffi:make-pointer (%id-of this))))))

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
      (adopt row (make-instance 'label :label label))
      (adopt row (make-instance 'label :label value-supplier :align :right))
      (adopt window row))))


(defmethod compose ((this debug-console))
  (with-slots (window) this
    (compose window)))
