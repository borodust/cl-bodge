(cl:in-package :cl-bodge.resources)


;;;
;;; Mesh chunk
;;;

(define-chunk-structure mesh-bone
  index
  offset
  bone)


(define-chunk-structure mesh-chunk
  face arrays indexes transform
  (bones mesh-bone))


(defmethod parse-chunk ((this (eql :mesh)) params data)
  (make-mesh-chunk data))
