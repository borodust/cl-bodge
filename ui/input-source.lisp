(cl:in-package :cl-bodge.ui)


(defgeneric next-keyboard-interaction (input-source))

(defgeneric next-mouse-interaction (input-source))

(defgeneric last-cursor-position (input-source &optional result-vec))

(defgeneric next-character (input-source))

(defgeneric next-scroll (input-source &optional result-vec))


(defstruct (bounded-queue
            (:constructor %make-bounded-queue))
  (head-idx 0 :type fixnum)
  (tail-idx 0 :type fixnum)
  (values (make-array 0) :type (simple-array t (*)) :read-only t))


(defun make-bounded-queue (size &rest array-initargs &key &allow-other-keys)
  (when (<= size 0)
    (error "size must be positive fixnum"))
  (%make-bounded-queue :values (apply #'make-array size array-initargs)))


(defun %next-array-index (array current-index)
  (let ((size (length (bounded-queue-values array))))
    (if (= current-index size) 0 (1+ current-index))))


(defun %next-array-head (bag)
  (let ((head (bounded-queue-head-idx bag))
        (tail (bounded-queue-tail-idx bag)))
    (unless (= head tail)
      (let ((next-head (%next-array-index bag head)))
        (if (= next-head tail)
            (setf (bounded-queue-head-idx bag) 0
                  (bounded-queue-tail-idx bag) 0)
            (setf (bounded-queue-head-idx bag) next-head)))
      head)))


(defun %next-array-tail (bag)
  (let* ((head (bounded-queue-head-idx bag))
         (tail (bounded-queue-tail-idx bag))
         (next-tail (%next-array-index bag tail)))
    (unless (= next-tail head)
      (setf (bounded-queue-tail-idx bag) next-tail)
      tail)))


(defun put-value (bag value)
  (when-let ((tail (%next-array-tail bag)))
    (setf (aref (bounded-queue-values bag) tail) value)
    t))


(defun pop-value (bag)
  (let ((head (bounded-queue-head-idx bag))
        (tail (bounded-queue-tail-idx bag)))
    (unless (or (= head tail) (not (%next-array-head bag)))
      (values (aref (bounded-queue-values bag) head) t))))


(defstruct (interaction-bag
            (:constructor %make-interaction-bag))
  (buttons nil :read-only t)
  (states nil :read-only t))


(defun make-interaction-bag (size)
  (when (<= size 0)
    (error "size must be positive fixnum"))
  (%make-interaction-bag :buttons (make-bounded-queue size
                                                       :element-type 'keyword
                                                       :initial-element :t)
                         :states (make-bounded-queue size
                                                      :element-type 'keyword
                                                      :initial-element :t)))


(defun register-interaction (bag button state)
  (when (put-value (interaction-bag-buttons bag) button)
    (put-value (interaction-bag-states bag) state)
    t))


(defun consume-interaction (bag)
  (when-let ((button (pop-value (interaction-bag-buttons bag))))
    (values button (pop-value (interaction-bag-states bag)))))


(defstruct (host-input-source
            (:constructor %make-host-input-source))
  (cursor-position (vec2) :read-only t)
  (scroll-offset (vec2) :read-only t)
  (keymap nil :read-only t)
  (character-bag nil :read-only t)
  (key-bag nil :read-only t)
  (button-bag nil :read-only t))


(defun make-host-input-source (&optional (bag-size 256))
  (let* ((key-bag (make-interaction-bag bag-size))
         (button-bag (make-interaction-bag bag-size))
         (keymap (ge.host:make-input-map))
         (character-bag (make-bounded-queue bag-size))
         (cursor-position (vec2))
         (scroll-offset (vec2)))
    (flet ((update-cursor-position (x y)
             (setf (x cursor-position) x
                   (y cursor-position) y))
           (register-keyboard-interaction (key state)
             (register-interaction key-bag key state))
           (register-mouse-interaction (button state)
             (register-interaction button-bag button state))
           (register-character (character)
             (put-value character-bag character ))
           (register-scroll (x-offset y-offset)
             (incf (x scroll-offset) x-offset)
             (incf (y scroll-offset) y-offset)))
      (ge.host:bind-cursor keymap #'update-cursor-position)
      (ge.host:bind-keyboard keymap #'register-keyboard-interaction)
      (ge.host:bind-mouse keymap #'register-mouse-interaction)
      (ge.host:bind-characters keymap #'register-character)
      (ge.host:bind-scroll keymap #'register-scroll))
    (%make-host-input-source :keymap keymap
                             :key-bag key-bag
                             :button-bag button-bag
                             :character-bag character-bag
                             :scroll-offset scroll-offset
                             :cursor-position cursor-position)))


(defun attach-host-input-source (input-source)
  (ge.host:enable-input-map (host-input-source-keymap input-source)))


(defun detach-host-input-source (input-source)
  (ge.host:disable-input-map (host-input-source-keymap input-source)))


(defmethod next-keyboard-interaction ((input-source host-input-source))
  (consume-interaction (host-input-source-key-bag input-source)))


(defmethod next-mouse-interaction ((input-source host-input-source))
  (consume-interaction (host-input-source-button-bag input-source)))


(defmethod last-cursor-position ((input-source host-input-source) &optional (result-vec (vec2)))
  (let ((cursor-position (host-input-source-cursor-position input-source)))
    (setf (x result-vec) (x cursor-position)
          (y result-vec) (y cursor-position))
    result-vec))


(defmethod next-character ((input-source host-input-source))
  (pop-value (host-input-source-character-bag input-source)))


(defmethod next-scroll ((input-source host-input-source) &optional (result-vec (vec2)))
  (let ((offset (host-input-source-scroll-offset input-source)))
    (setf (x result-vec) (x offset)
          (y result-vec) (y offset)
          (x offset) 0f0
          (y offset) 0f0)
    result-vec))
