(in-package :cl-bodge.interactions)


(defstruct input-state
  (lock (bt:make-lock "input-lock"))
  (cursor-position (vec2) :type vec2 :read-only t)
  (cursor-offset (vec2) :type vec2)
  (mouse-left-button :released :type keyword)
  (mouse-right-button :released :type keyword)
  (backspace-clicks 0 :type integer)
  (character-stream (list)))


(defun register-character (input character)
  (bt:with-lock-held ((input-state-lock input))
    (alexandria:nconcf (input-state-character-stream input) (list character))))


(defun read-characters (input)
  (bt:with-lock-held ((input-state-lock input))
    (prog1 (input-state-character-stream input)
      (setf (input-state-character-stream input) nil))))


(defun register-key-action (input key action)
  (when (and (eq key :backspace) (eq action :pressed))
    (bt:with-lock-held ((input-state-lock input))
      (incf (input-state-backspace-clicks input)))))


(defun read-backspace-click (input)
  (bt:with-lock-held ((input-state-lock input))
    (unless (= 0 (input-state-backspace-clicks input))
      (decf (input-state-backspace-clicks input))
      t)))


(defun register-cursor-state (state x y)
  (let ((position (input-state-cursor-position state)))
    (setf (x position) (f x)
          (y position) (f y))))


(defun read-cursor-state (input)
  (bt:with-lock-held ((input-state-lock input))
    (let ((position (input-state-cursor-position input)))
      (values (x position)
              (y position)))))


(defun read-cursor-offset (input)
  (bt:with-lock-held ((input-state-lock input)) ;; fixme: may not be needed here
    (prog1
        (input-state-cursor-offset input)
      (setf (input-state-cursor-offset input) (vec2)))))


(defun register-mouse-state (input button state)
  (bt:with-lock-held ((input-state-lock input))
    (case button
      (:left (setf (input-state-mouse-left-button input) state))
      (:right (setf (input-state-mouse-right-button input) state)))))


(defun read-mouse-state (input button)
  (bt:with-lock-held ((input-state-lock input))
    (ecase button
      (:left (input-state-mouse-left-button input))
      (:right (input-state-mouse-right-button input)))))
