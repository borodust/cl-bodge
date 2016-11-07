(in-package :cl-bodge.resources)

;;;
;;;
;;;
(define-chunk-structure (mesh-chunk)
  face arrays indexes)


(defmethod parse-chunk ((this (eql :mesh)) params data)
  (make-mesh-chunk data))


(defun mesh-chunks-of (resource)
  (chunks-by-type resource :mesh))


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


(defun text-shader-chunks-of (resource)
  (chunks-by-type resource :text-shader))


;;;
;;;
;;;

(define-chunk-structure (skeleton-bone t skeleton-bone)
  transform)


(define-chunk-structure (skeleton-chunk t skeleton-bone))


(defmethod parse-chunk ((this (eql :skeleton)) params data)
  (make-skeleton-chunk data))


(defun skeleton-chunks-of (resource)
  (chunks-by-type resource :skeleton))
