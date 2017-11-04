(in-package :cl-bodge.assets)


;;;
;;; Mesh resource
;;;

(defstruct mesh-asset
  (mesh nil :read-only t)
  (transform nil :read-only t)
  (bones nil :read-only t))


(define-system-function build-mesh ge.gx:graphics-system (mesh-chunk)
  (let* ((arrays (mesh-chunk-arrays mesh-chunk))
         (v-count (length (cadar arrays)))
         (chunk-bones (mesh-chunk-bones mesh-chunk))
         (bone-count (reduce #'max chunk-bones :key #'mesh-bone-index :initial-value 0))
         (bones
          (loop with r = (make-array bone-count :initial-element nil)
             for bone in chunk-bones do
               (setf (aref r (1- (mesh-bone-index bone)))
                     (when-let ((skeleton-bone (mesh-bone-bone bone)))
                       (cons (id-of skeleton-bone)
                             (sequence->mat4 (mesh-bone-offset bone)))))
             finally (return r)))
         (index-array (list->array (mesh-chunk-indexes mesh-chunk)))
         (mesh (ge.gx:make-mesh v-count :triangles index-array)))
    (loop for (array-id array) in arrays do
         (with-disposable ((vbuf (ge.gx:make-array-buffer
                                  (list->array array v-count (length (car array))))))
           (ge.gx:attach-array-buffer vbuf mesh array-id)))
    (values mesh (sequence->mat4 (mesh-chunk-transform mesh-chunk)) bones)))

;;;
;;; Resource handler
;;;
(defclass mesh-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :mesh))


(defmethod make-resource-handler ((type (eql :mesh)) &key)
  (make-instance 'mesh-resource-handler))
