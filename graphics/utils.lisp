(cl:in-package :cl-bodge.graphics)


(defun use-texture-unit (val)
  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) val)))


(defmacro with-texture-unit ((unit-id) &body body)
  (once-only (unit-id)
    `(unwind-protect
          (progn
            (use-texture-unit ,unit-id)
            ,@body)
       (use-texture-unit 0))))


(defmacro with-bound-texture ((target texture-id) &body body)
  (once-only (target)
    `(unwind-protect
          (progn
            (gl:bind-texture ,target ,texture-id)
            ,@body)
       (gl:bind-texture ,target 0))))


(defun dispose-gl-object (object destructor)
  (run (flow:-> (engine-system 'graphics-system) :disposing t ()
         (funcall destructor object))))
