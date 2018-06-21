(cl:in-package :cl-bodge.graphics)


(defun render (output pipeline &rest input
               &key index-array
                 (instance-count 1)
                 vertex-count
                 (vertex-offset 0)
               &allow-other-keys)
  (enable-rendering-output output)
  (enable-pipeline pipeline input)
  (let ((mode (pipeline-primitive pipeline)))
    (if index-array
        (progn
          (inject-shader-input index-array)
          (%gl:draw-elements-instanced mode
                                       (or vertex-count (index-array-length index-array))
                                       :unsigned-int
                                       (cffi:make-pointer vertex-offset)
                                       instance-count))
        (%gl:draw-arrays-instanced mode vertex-offset vertex-count instance-count))))
