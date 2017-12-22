(in-package :cl-bodge.host)


(defclass input-map (subscribing)
  ((cursor-action :initform (make-guarded-reference nil))
   (key-table :initform (make-guarded-reference (make-hash-table :test 'eq)))
   (button-table :initform (make-guarded-reference (make-hash-table :test 'eq)))
   (key-interceptor :initform nil)
   (button-interceptor :initform nil)))


(defmethod initialize-instance :after ((input-map input-map) &key)
  (with-slots (cursor-action key-table button-table
               key-interceptor button-interceptor)
      input-map
    (flet ((register-callback (class action)
             (add-event-handler input-map class action))
           (process-key-event (ev)
             (when key-interceptor
               (funcall key-interceptor (key-from ev) (state-from ev)))
             (when-let ((action (with-guarded-reference (key-table)
                                  (gethash (key-from ev) key-table))))
               (funcall action (state-from ev))))
           (process-button-event (ev)
             (when button-interceptor
               (funcall button-interceptor (button-from ev) (state-from ev)))
             (when-let ((action (with-guarded-reference (button-table)
                                  (gethash (button-from ev) button-table))))
               (funcall action (state-from ev))))
           (process-cursor-event (ev)
             (when-let ((action (guarded-value-of cursor-action)))
               (funcall action (x-from ev) (y-from ev)))))
      (register-callback 'keyboard-event #'process-key-event)
      (register-callback 'mouse-event #'process-button-event)
      (register-callback 'cursor-event #'process-cursor-event))))


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


(defun bind-mouse (input-map action)
  (with-slots (button-interceptor) input-map
    (setf button-interceptor action)))


(defun bind-keyboard (input-map action)
  (with-slots (key-interceptor) input-map
    (setf key-interceptor action)))


(defun bind-cursor (input-map action)
  (with-slots (cursor-action) input-map
    (with-guarded-reference (cursor-action)
      (setf cursor-action action))))
