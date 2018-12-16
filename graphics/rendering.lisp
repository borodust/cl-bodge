(cl:in-package :cl-bodge.graphics)


(defun render (output pipeline &rest input
               &key index-buffer
                 (instance-count 1)
                 vertex-count
                 (vertex-offset 0)
                 primitive
               &allow-other-keys)
  (flet ((%render ()
           (apply #'render-pipeline pipeline
                  :index-buffer index-buffer
                  :instance-count instance-count
                  :vertex-count vertex-count
                  :vertex-offset vertex-offset
                  :primitive primitive
                  input)))
    (run-with-bound-output output #'%render)))


(defun finish-rendering-output ()
  (gl:finish))
