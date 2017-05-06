(in-package :cl-bodge.host)


(defclass keymap (event-listening)
  ((cursor-action :initform (make-guarded-reference nil))
   (key-table :initform (make-guarded-reference (make-hash-table :test 'eq)))
   (button-table :initform (make-guarded-reference (make-hash-table :test 'eq)))))


(defmethod initialize-instance :after ((keymap keymap) &key)
  (with-slots (cursor-action key-table button-table) keymap
    (let ((host (host)))
      (flet ((register-callback (class action)
               (register-event-handler keymap class host action))
             (process-key-event (ev)
               (when-let ((action (with-guarded-reference (key-table)
                                    (gethash (key-from ev) key-table))))
                 (funcall action (state-from ev))))
             (process-button-event (ev)
               (when-let ((action (with-guarded-reference (button-table)
                                    (gethash (button-from ev) button-table))))
                 (funcall action (state-from ev))))
             (process-cursor-event (ev)
               (when-let ((action (guarded-value-of cursor-action)))
                 (funcall action (x-from ev) (y-from ev)))))
        (register-callback 'keyboard-event #'process-key-event)
        (register-callback 'mouse-event #'process-button-event)
        (register-callback 'cursor-event #'process-cursor-event)))))


(defun make-keymap ()
  (make-instance 'keymap))


(defun enable-keymap (keymap)
  (subscribe-listener keymap))


(defun disable-keymap (keymap)
  (unsubscribe-listener keymap))


(defun bind-button (keymap button action)
  (with-slots (button-table) keymap
    (with-guarded-reference (button-table)
      (setf (gethash button button-table) action))))


(defun bind-key (keymap key action)
  (with-slots (key-table) keymap
    (with-guarded-reference (key-table)
      (setf (gethash key key-table) action))))


(defun bind-cursor (keymap action)
  (with-slots (cursor-action) keymap
    (with-guarded-reference (cursor-action)
      (setf cursor-action action))))
