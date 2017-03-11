(in-package :cl-bodge.interactions)


(defstruct input-state
  (lock (bt:make-lock "input-lock"))
  (cursor-position (vec2) :type vec2)
  (cursor-offset (vec2) :type vec2)
  (mouse-left-button :released :type keyword)
  (mouse-right-button :released :type keyword)
  (backspace-clicks 0 :type integer)
  (character-stream (list)))


(defun register-character (input character)
  (bt:with-lock-held ((input-state-lock input))
    (alexandria:nconcf (input-state-character-stream input) (list character))))


(defun read-character (input)
  (bt:with-lock-held ((input-state-lock input))
    (pop (input-state-character-stream input))))


(defun register-key-action (input key action)
  (when (and (eq key :backspace) (eq action :pressed))
    (bt:with-lock-held ((input-state-lock input))
      (incf (input-state-backspace-clicks input)))))


(defun read-backspace-click (input)
  (bt:with-lock-held ((input-state-lock input))
    (unless (= 0 (input-state-backspace-clicks input))
      (decf (input-state-backspace-clicks input))
      t)))


(defun read-cursor-offset (input)
  (bt:with-lock-held ((input-state-lock input)) ;; fixme: may not be needed here
    (prog1
        (input-state-cursor-offset input)
      (setf (input-state-cursor-offset input) (vec2)))))
