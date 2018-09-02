(cl:in-package :bodge-host)


(defenum hotkey-state
  :pressed
  :released)


(defclass hotkey-listener ()
  ((hotkey-table :initform (make-guarded-reference (make-hash-table :test #'equal)))
   (current-bag :initform (make-guarded-reference nil))))


(defun %ensure-correct-hotkey (key-bag)
  (stable-sort key-bag #'string<))


(defun make-hotkey-listener ()
  (make-instance 'hotkey-listener))


(defun register-keyboard-hotkey (hotkey-listener key-bag action)
  (with-slots (hotkey-table) hotkey-listener
    (with-guarded-reference (hotkey-table)
      (setf (gethash (%ensure-correct-hotkey key-bag) hotkey-table) action))))


(defun %get-hotkey-action (listener key-bag)
  (with-slots (hotkey-table) listener
    (with-guarded-reference (hotkey-table)
      (gethash key-bag hotkey-table))))


(defun hotkey-update-key-state (hotkey-listener key state)
  (with-slots (hotkey-table current-bag) hotkey-listener
    (unless (eq state :repeating)
      (with-guarded-reference (current-bag)
        (when-let ((prev-hotkey-action (%get-hotkey-action hotkey-listener
                                                           current-bag)))
          (funcall prev-hotkey-action :released))
        (let ((new-bag (case state
                         (:pressed (%ensure-correct-hotkey
                                    (cons key current-bag)))
                         (:released (delete key current-bag)))))
          (unwind-protect
               (when-let ((new-hotkey-action (%get-hotkey-action hotkey-listener
                                                                 new-bag)))
                 (funcall new-hotkey-action :pressed))
            (setf current-bag new-bag)))))))
