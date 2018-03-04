(cl:in-package :cl-bodge.host)

(defenum keyboard-key
  :unknown
  :space :apostrophe :comma :minus :period :slash
  :0 :1 :2 :3 :4 :5 :6 :7 :8 :9
  :semicolon :equal
  :a :b :c :d :e :f :g :h :i :j :k :l :m :n :o :p :q :r :s :t :u :v :w :x :y :z
  :left-bracket :backslash :right-bracket :grave-accent :world-1 :world-2
  :escape :enter :tab :backspace :insert :delete :right :left :down :up
  :page-up :page-down :home :end :caps-lock :scroll-lock :num-lock :print-screen :pause
  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
  :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25
  :keypad-0 :keypad-1 :keypad-2 :keypad-3 :keypad-4
  :keypad-5 :keypad-6 :keypad-7 :keypad-8 :keypad-9
  :keypad-decimal :keypad-divide :keypad-multiply
  :keypad-subtract :keypad-add :keypad-enter :keypad-equal
  :left-shift :left-control :left-alt :left-super
  :right-shift :right-control :right-alt :right-super
  :menu)


(defenum button-state
  :pressed :released :repeating)


(defenum keyboard-key-modifier
  :shift :control :alt :super)


(defenum mouse-button
  :unknown :left :right :middle)


(defvar *mouse-button-map* (make-hash-table-with-entries (:test #'eql)
                             (%glfw:+mouse-button-left+ :left)
                             (%glfw:+mouse-button-right+ :right)
                             (%glfw:+mouse-button-middle+ :middle)
                             (:left %glfw:+mouse-button-left+)
                             (:right %glfw:+mouse-button-right+)
                             (:middle %glfw:+mouse-button-middle+)))


(macrolet ((make-key-map ()
             (flet ((ensure-keyword (value)
                      (case value
                        (:kp-0 :keypad-0)
                        (:kp-1 :keypad-1)
                        (:kp-2 :keypad-2)
                        (:kp-3 :keypad-3)
                        (:kp-4 :keypad-4)
                        (:kp-5 :keypad-5)
                        (:kp-6 :keypad-6)
                        (:kp-7 :keypad-7)
                        (:kp-8 :keypad-8)
                        (:kp-9 :keypad-9)
                        (:kp-decimal :keypad-decimal)
                        (:kp-divide :keypad-divide)
                        (:kp-multiply :keypad-multiply)
                        (:kp-subtract :keypad-subtract)
                        (:kp-add :keypad-add)
                        (:kp-enter :keypad-enter)
                        (:kp-equal :keypad-equal)
                        (t value))))
               `(make-hash-table-with-entries (:test #'eql)
                  ,@(loop with prefix = (symbol-name '#:+key-)
                          for name being the symbol in (find-package :%glfw)
                          as symbol-name = (symbol-name name)
                          when (alexandria:starts-with-subseq prefix symbol-name)
                            append (let* ((symbol-name (subseq symbol-name 5
                                                               (position #\+ symbol-name :start 1)))
                                          (keyword (ensure-keyword
                                                    (alexandria:make-keyword symbol-name))))
                                     `((,(symbol-value name) ,keyword)
                                       (,keyword ,(symbol-value name)))))))))
  (defvar *key-map* (make-key-map)))


(defun glfw-enumval->keyboard-key (value)
  (gethash value *key-map* :unknown))


(defun keyboard-key->glfw-enumval (value)
  (gethash value *key-map* %glfw:+key-unknown+))


(defun glfw-enumval->button-state (value)
  (eswitch (value :test #'=)
    (%glfw:+press+ :pressed)
    (%glfw:+release+ :released)
    (%glfw:+repeat+ :repeating)))


(defun glfw-enumval->mouse-button (value)
  (gethash value *mouse-button-map* :unknown))


(defun mouse-button->glfw-enumval (value)
  (gethash value *mouse-button-map* %glfw:+key-unknown+))


(defevent input-event () ())


(defevent keyboard-event (input-event)
  (key state))


(defevent character-input-event (input-event)
  (character))


(defevent mouse-event (input-event)
  (button state))


(defevent cursor-event (input-event)
  (x y))


(defevent viewport-size-change-event ()
  (width height))


(defevent framebuffer-size-change-event ()
  (width height))


(defevent scroll-event (input-event)
  (x-offset y-offset))


(defevent viewport-hiding-event () ())
