(cl:in-package :cl-bodge.graphics)


(declaim (special *state*
                  *state-slice*))


(defclass state-slice ()
  ((mutation-table :initform (make-hash-table :test 'eql))
   (evolution-list :initform (list))))


(defun push-mutator (state-slice id mutator)
  (with-slots (mutation-table evolution-list) state-slice
    (setf (gethash id mutation-table) mutator)
    (push mutator evolution-list)))


(defun find-mutator (state-slice id)
  (with-slots (mutation-table) state-slice
    (gethash id mutation-table)))


(defun apply-slice-mutators (state-slice)
  (with-slots (evolution-list) state-slice
    (loop for mutator in (reverse evolution-list)
       do (funcall mutator))))


(defun mutator-id-list (state-slice)
  (with-slots (mutation-table) state-slice
    (loop for k being the hash-key of mutation-table
       collect k)))


(defmacro with-state-slice ((slice) &body body)
  `(let ((*state-slice* ,slice))
     ,@body))


;;;
;;;
;;;
(defclass context-state ()
  ((mutating-stack :initform (list (make-instance 'state-slice)))))


(defun find-state-mutator (state mutator-id)
  (with-slots (mutating-stack) state
    (loop for slice in mutating-stack
       thereis (find-mutator slice mutator-id))))


(defun preserve-state (state)
  (with-slots (mutating-stack) state
    (let ((new-slice (make-instance 'state-slice)))
      (push new-slice mutating-stack)
      new-slice)))


(defun restore-state (state)
  (with-slots (mutating-stack) state
    (let ((state-slice (pop mutating-stack)))
      (loop for mutator-id in (mutator-id-list state-slice)
         do (when-let ((mutator (find-state-mutator state mutator-id)))
              (funcall mutator))))))


(defun current-state-slice (state)
  (with-slots (mutating-stack) state
    (first mutating-stack)))


(defun %reset-managed-state ()
  (gl:use-program 0)

  ;; fixme: reset other units
  (use-texture-unit 0)
  (gl:bind-texture :texture-2d 0)

  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :element-array-buffer 0)
  (gl:bind-buffer :uniform-buffer 0))


(defun reset-context-state (state)
  (with-slots (mutating-stack) state
    (loop for slice in (reverse mutating-stack)
       do (apply-slice-mutators slice))
    (%reset-managed-state)))


(defmacro with-current-state-slice ((state) &body body)
  `(with-state-slice ((current-state-slice ,state))
     ,@body))


(defmacro export-mutator (name (&rest parameters))
  (let* ((mutator-name (format-symbol :gx.state "~A" name))
         (arg-list (apply-argument-list parameters)))
    (with-gensyms (mutator)
      `(progn
         (defun ,mutator-name (,@parameters)
           (flet ((,mutator () (apply #',name ,@arg-list)))
             (,mutator)
             (push-mutator *state-slice* ',name #',mutator)))
         (export ',mutator-name :gx.state)))))


(defun gx.state::enable (&rest features)
  (apply #'gl:enable features)
  (loop for feature in features
     do (let ((feature feature))
          (push-mutator *state-slice* feature (lambda () (gl:enable feature))))))


(defun gx.state::disable (&rest features)
  (apply #'gl:disable features)
  (loop for feature in features
     do (let ((feature feature))
          (push-mutator *state-slice* feature (lambda () (gl:disable feature))))))


;;;
;;; GLOBAL STATE only
;;;

(export-mutator gl:cull-face (face))
(export-mutator gl:front-face (mode))
(export-mutator gl:clear-color (red green blue alpha))
(export-mutator gl:color-mask (red green blue alpha))
(export-mutator gl:clear-depth (value))
(export-mutator gl:blend-func (source-factor destination-factor))
(export-mutator gl:blend-func-separate (source-factor-rgb
                                        destination-factor-rgb
                                        source-factor-alpha
                                        destination-factor-alpha))
(export-mutator gl:clear-stencil (value))
(export-mutator gl:stencil-mask (value))
(export-mutator gl:stencil-func (function reference mask))
(export-mutator gl:stencil-op (fail z-fail z-pass))
