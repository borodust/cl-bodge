(in-package :cl-bodge.graphics)


(defmacro foreign-handle (symbol default)
  `(if-bound ,symbol
             (handle-value-of ,symbol)
             ,default))


(defun reset-state ()
  (gl:enable :blend
             :cull-face
             :depth-test
             :program-point-size)
  (gl:disable :scissor-test)

  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (gl:use-program (foreign-handle *active-shading-program* 0))

  (gl:color-mask t t t t)
  (gl:stencil-mask #xffffffff)
  (gl:stencil-op :keep :keep :keep)

  (gl:stencil-func :always 0 #xffffffff)

  ;; fixme: reset others units
  (use-texture-unit (bound-symbol-value *active-texture-unit* 0))
  (if-bound *active-texture*
            (when (eq (target-of *active-texture*) :texture-2d)
              (use-texture *active-texture*))
            (gl:bind-texture :texture-2d 0))

  (if-bound *active-buffer*
            (when (eq :array-buffer (target-of *active-buffer*))
              (use-buffer *active-buffer*))
            (gl:bind-buffer :array-buffer 0))

  (gl:bind-buffer :uniform-buffer 0)

  (gl:bind-vertex-array (foreign-handle *active-vertex-array* 0)))
