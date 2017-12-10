(in-package :cl-bodge.resources)

;;;
;;; Sekeleton chunk
;;;

(define-chunk-structure (skeleton-bone t skeleton-bone)
  transform)


(defmethod parse-chunk ((this (eql :skeleton)) params data)
  (make-skeleton-bone data))
