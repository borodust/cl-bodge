(cl:in-package :cl-bodge.graphics)


(defmethod inject-shader-input ((value integer) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (gl:uniformi location value)))


(defmethod inject-shader-input ((value single-float) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (%gl:uniform-1f location value)))


(defmethod inject-shader-input ((value double-float) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (%gl:uniform-1d location value)))


(defmethod inject-shader-input ((value vec) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (gl:uniformfv location (vec->array value))))


(defmethod inject-shader-input ((value mat4) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-4fv location 1 nil ptr))))


(defmethod inject-shader-input ((value mat3) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-3fv location 1 nil ptr))))


(defmethod inject-shader-input ((value mat2) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-2fv location 1 nil ptr))))
