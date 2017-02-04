(in-package :cl-bodge.scene)


(declaim (special *lights*))


(defclass light-node (scene-node)
  ((light :initarg :light :initform nil)))


(defmethod scene-pass ((this light-node) (pass rendering-pass) input)
  (with-slots (light) this
    (let ((*lights* (if-bound *lights*
                              (push light *lights*)
                              (list light))))
      (unwind-protect
           (call-next-method)
        (pop *lights*)))))
