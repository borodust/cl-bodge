(in-package :cl-bodge.resources)


;;;
;;;
;;;
(define-chunk-structure (simple-model-bone t simple-model-bone)
  rotation translation)


(define-chunk-structure (simple-model-skeleton t simple-model-bone))


(define-chunk-structure (simple-model-sequence t)
  (bone :reference))


(define-chunk-structure (simple-model-keyframe-animation t simple-model-sequence))


(define-chunk-structure (simple-model-program)
  parameters
  (shaders :reference))


(define-chunk-structure (simple-model-chunk)
  (meshes :reference)
  (skeleton simple-model-skeleton)
  (keyframe-animations simple-model-keyframe-animation)
  (programs simple-model-program))


(defmethod parse-chunk ((chunk-type (eql :simple-model)) params data)
  (make-simple-model-chunk data))


(defun simple-model-chunks-of (resource)
  (chunks-by-type resource :simple-model))
