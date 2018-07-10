(cl:in-package :cl-bodge.graphics)


;;;
;;; 2D TEXTURES
;;;
(defun run-with-stray-2d-color-attachment (texture-target texture-id d-width d-height function)
  (gl:bind-framebuffer :framebuffer *supplementary-framebuffer*)
  (gl:draw-buffer :color-attachment0)
  (gl:framebuffer-texture-2d :framebuffer (index->color-attachment 0)
                             texture-target texture-id 0)
  (gl:bind-renderbuffer :renderbuffer *depth-stencil-renderbuffer*)
  (gl:renderbuffer-storage :renderbuffer :depth-stencil d-width d-height)
  (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                               :renderbuffer *depth-stencil-renderbuffer*)
  (gl:viewport 0 0 d-width d-height)
  (gl:clear :depth-buffer :stencil-buffer)
  (unwind-protect
       (funcall function)
    (gl:framebuffer-texture-2d :framebuffer (index->color-attachment 0)
                               texture-target 0 0)
    (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment
                                 :renderbuffer 0)))


(defmethod %clear-rendering-output ((this texture-2d) color)
  (flet ((%clear ()
           (gl:clear-color (x color) (y color) (z color) (w color))
           (gl:clear :color-buffer :depth-buffer :stencil-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod run-with-bound-output ((texture texture-2d) function)
  (destructuring-bind (width height) (texture-dimensions texture)
    (run-with-stray-2d-color-attachment (%target-of texture)
                                        (%texture-id-of texture)
                                        width height
                                        function)))


(defmethod %clear-rendering-output ((this depth-texture) color)
  (flet ((%clear ()
           (gl:clear :depth-buffer)))
    (run-with-bound-output this #'%clear)))


(defun run-with-bound-depth-output (texture-id texture-target texture-dimensions function)
  (gl:bind-framebuffer :framebuffer *supplementary-framebuffer*)
  (gl:draw-buffer :none)
  (gl:framebuffer-texture-2d :framebuffer :depth-attachment texture-target texture-id 0)
  (destructuring-bind (width height) texture-dimensions
    (gl:viewport 0 0 width height))
  (unwind-protect
       (funcall function)
    (gl:framebuffer-texture-2d :framebuffer  :depth-attachment texture-target 0 0)))


(defmethod run-with-bound-output ((texture depth-texture) function)
  (run-with-bound-depth-output (%texture-id-of texture) (%target-of texture)
                               (texture-dimensions texture) function))


;;;
;;; CUBEMAPS
;;;
(defvar *positive-x* (mat4 0 0 -1 0
                           0 -1 0 0
                           -1 0 0 0
                           0 0 0 1))

(defvar *positive-y* (mat4 1 0 0 0
                           0 0 1 0
                           0 -1 0 0
                           0 0 0 1))

(defvar *positive-z* (mat4 1 0 0 0
                           0 -1 0 0
                           0 0 -1 0
                           0 0 0 1))

(defvar *negative-x* (mat4 0 0 1 0
                           0 -1 0 0
                           1 0 0 0
                           0 0 0 1))

(defvar *negative-y* (mat4 1 0 0 0
                           0 0 -1 0
                           0 1 0 0
                           0 0 0 1))

(defvar *negative-z* (mat4 -1 0 0 0
                           0 -1 0 0
                           0 0 1 0
                           0 0 0 1))

(defmethod %clear-rendering-output ((this cubemap-texture-layer) color)
  (flet ((%clear ()
           (gl:clear-color (x color) (y color) (z color) (w color))
           (gl:clear :color-buffer :depth-buffer :stencil-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod %clear-rendering-output ((this cubemap-texture) color)
  (%clear-rendering-output (cubemap-positive-x-layer this) color)
  (%clear-rendering-output (cubemap-positive-y-layer this) color)
  (%clear-rendering-output (cubemap-positive-z-layer this) color)
  (%clear-rendering-output (cubemap-negative-x-layer this) color)
  (%clear-rendering-output (cubemap-negative-y-layer this) color)
  (%clear-rendering-output (cubemap-negative-z-layer this) color))


(defmethod run-with-bound-output ((layer cubemap-texture-layer) function)
  (destructuring-bind (width height) (texture-dimensions (%texture-of layer))
    (run-with-stray-2d-color-attachment (%layer-type-of layer)
                                        (%texture-id-of (%texture-of layer))
                                        width height
                                        function)))


(defmethod %clear-rendering-output ((this depth-cubemap-texture-layer) color)
  (declare (ignore color))
  (flet ((%clear ()
           (gl:clear ::depth-buffer)))
    (run-with-bound-output this #'%clear)))


(defmethod run-with-bound-output ((layer depth-cubemap-texture-layer) function)
  (let ((texture (%texture-of layer)))
    (run-with-bound-depth-output (%texture-id-of texture) (%layer-type-of layer)
                                 (texture-dimensions texture) function)))


(defun render-to-cubemap (layer rotation rendering-routine)
  (funcall rendering-routine layer rotation))


(defun for-each-cubemap-layer (cubemap rendering-routine)
  (render-to-cubemap (cubemap-positive-x-layer cubemap)
                     *positive-x*
                     rendering-routine)
  (render-to-cubemap (cubemap-positive-y-layer cubemap)
                     *positive-y*
                     rendering-routine)
  (render-to-cubemap (cubemap-positive-z-layer cubemap)
                     *positive-z*
                     rendering-routine)
  (render-to-cubemap (cubemap-negative-x-layer cubemap)
                     *negative-x*
                     rendering-routine)
  (render-to-cubemap (cubemap-negative-y-layer cubemap)
                     *negative-y*
                     rendering-routine)
  (render-to-cubemap (cubemap-negative-z-layer cubemap)
                     *negative-z*
                     rendering-routine))


(defmacro do-cubemap-layers (((layer rotation) cubemap) &body body)
  (once-only (cubemap)
    `(for-each-cubemap-layer ,cubemap (lambda (,layer ,rotation) ,@body))))
