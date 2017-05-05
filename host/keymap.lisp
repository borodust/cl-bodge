(in-package :cl-bodge.host)


;; fixme: don't forget to make thread safe for generalization
(defclass keymap (lockable)
  ((callbacks :initform nil)
   (cursor-action :initform nil)
   (key-table :initform (make-hash-table :test 'eq))))


(defun mouse-button->keymap-button (button)
  (case button
    (:left :mouse-left)
    (:right :mouse-right)
    (:middle :mouse-middle)))


(defun enable-keymap (keymap)
  (with-slots (callbacks cursor-action key-table) keymap
    (when callbacks
      (error "Keymap already enabled"))
    (let ((host (host)))
      (flet ((register-callback (class action)
               (push (cons class (subscribe-to class host action)) callbacks))
             (process-key-event (ev)
               (when-let ((action (gethash (key-from ev) key-table)))
                 (funcall action (state-from ev))))
             (process-button-event (ev)
               (let ((button (mouse-button->keymap-button (button-from ev))))
                 (when-let ((action (gethash button key-table)))
                   (funcall action (state-from ev)))))
             (process-cursor-event (ev)
               (when cursor-action
                 (funcall cursor-action (x-from ev) (y-from ev)))))
        (register-callback 'keyboard-event #'process-key-event)
        (register-callback 'mouse-event #'process-button-event)
        (register-callback 'cursor-event #'process-cursor-event)))))


(defun disable-keymap (keymap)
  (with-slots (callbacks) keymap
    (unless callbacks
      (error "Keymap already disabled"))
    (loop with host = (host)
       for (class . cb) in callbacks
       do (unsubscribe-from class cb host))
    (setf callbacks nil)))


(defun bind-button (keymap button action)
  (with-slots (key-table) keymap
    (setf (gethash button key-table) action)))


(defun bind-cursor (keymap action)
  (with-slots (cursor-action) keymap
    (setf cursor-action action)))
