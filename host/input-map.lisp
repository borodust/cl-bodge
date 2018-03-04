(cl:in-package :cl-bodge.host)


(defclass input-map (subscribing)
  ((cursor-action :initform (make-guarded-reference nil))
   (scroll-action :initform (make-guarded-reference nil))
   (key-table :initform (make-guarded-reference (make-hash-table :test 'eq)))
   (button-table :initform (make-guarded-reference (make-hash-table :test 'eq)))
   (key-action :initform nil)
   (character-action :initform nil)
   (button-action :initform nil)
   (hotkey-listener :initform nil)))


(defmethod initialize-instance :after ((input-map input-map) &key)
  (with-slots (cursor-action key-table button-table hotkey-listener
               key-action button-action character-action scroll-action)
      input-map
    (flet ((register-callback (class action)
             (add-event-handler input-map class action))
           (process-key-event (ev)
             (when hotkey-listener
               (hotkey-update-key-state hotkey-listener
                                        (key-from ev) (state-from ev)))
             (when key-action
               (funcall key-action (key-from ev) (state-from ev)))
             (when-let ((action (with-guarded-reference (key-table)
                                  (gethash (key-from ev) key-table))))
               (funcall action (state-from ev))))
           (process-button-event (ev)
             (when button-action
               (funcall button-action (button-from ev) (state-from ev)))
             (when-let ((action (with-guarded-reference (button-table)
                                  (gethash (button-from ev) button-table))))
               (funcall action (state-from ev))))
           (process-cursor-event (ev)
             (when-let ((action (guarded-value-of cursor-action)))
               (funcall action (x-from ev) (y-from ev))))
           (process-scroll-event (ev)
             (when-let ((action (guarded-value-of scroll-action)))
               (funcall action (x-offset-from ev) (y-offset-from ev))))
           (process-character-event (ev)
             (when character-action
               (funcall character-action (character-from ev)))))
      (register-callback 'keyboard-event #'process-key-event)
      (register-callback 'mouse-event #'process-button-event)
      (register-callback 'cursor-event #'process-cursor-event)
      (register-callback 'scroll-event #'process-scroll-event)
      (register-callback 'character-input-event #'process-character-event))))


(defun %ensure-hotkey-listener (input-map)
  (with-slots (hotkey-listener) input-map
    (if (null hotkey-listener)
        (setf hotkey-listener (make-hotkey-listener))
        hotkey-listener)))


(defun make-input-map ()
  (make-instance 'input-map))


(defun enable-input-map (input-map)
  (employ-subscriber input-map))


(defun disable-input-map (input-map)
  (dismiss-subscriber input-map))


(defun bind-mouse-button (input-map button action)
  (with-slots (button-table) input-map
    (with-guarded-reference (button-table)
      (setf (gethash button button-table) action))))


(defun bind-keyboard-button (input-map key action)
  (with-slots (key-table) input-map
    (with-guarded-reference (key-table)
      (setf (gethash key key-table) action))))


(defun bind-keyboard-hotkey (input-map key-bag action)
  (let ((listener (%ensure-hotkey-listener input-map)))
    (register-keyboard-hotkey listener key-bag action)))


(defun bind-mouse (input-map action)
  (with-slots (button-action) input-map
    (setf button-action action)))


(defun bind-keyboard (input-map action)
  (with-slots (key-action) input-map
    (setf key-action action)))


(defun bind-characters (input-map action)
  (with-slots (character-action) input-map
    (setf character-action action)))


(defun bind-cursor (input-map action)
  (with-slots (cursor-action) input-map
    (with-guarded-reference (cursor-action)
      (setf cursor-action action))))


(defun bind-scroll (input-map action)
  (with-slots (scroll-action) input-map
    (with-guarded-reference (scroll-action)
      (setf scroll-action action))))
