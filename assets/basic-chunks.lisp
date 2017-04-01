(in-package :cl-bodge.assets)

;;;
;;;
;;;
(define-chunk-structure mesh-bone
  index
  offset
  (bone :reference))


(define-chunk-structure mesh-chunk
  face arrays indexes transform
  (bones mesh-bone))


(defmethod parse-chunk ((this (eql :mesh)) params data)
  (make-mesh-chunk data))


;;;
;;;
;;;
(defstruct text-shader-chunk
  id type source)


(defmethod parse-chunk ((chunk-type (eql :text-shader)) params data)
  (destructuring-bind (&key name type) params
    (let ((text (if (listp data)
                    (read-file-into-string (merge-relative (second data)))
                    data)))
      (push-object name (make-text-shader-chunk :id name :type type :source text)))))


;;;
;;;
;;;

(define-chunk-structure (skeleton-bone t skeleton-bone)
  transform)


(defmethod parse-chunk ((this (eql :skeleton)) params data)
  (make-skeleton-bone data))


;;;
;;;
;;;

(define-chunk-structure (keyframe-sequence t)
  (bone :reference))


(define-chunk-structure (animation-chunk t keyframe-sequence))


(defmethod parse-chunk ((this (eql :animation)) params data)
  (make-animation-chunk data))
