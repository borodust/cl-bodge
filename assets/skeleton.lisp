(in-package :cl-bodge.assets)


(defun chunk->skeleton (chunk)
  (labels ((%traverse (bone)
             (let ((node (make-instance 'ge.sg:bone-node
                                        :name (id-of bone)
                                        :transform (sequence->mat4
                                                    (skeleton-bone-transform bone)))))
               (dolist (child (children-of bone))
                 (adopt node (%traverse child)))
               node)))
    (unless (null chunk)
      (%traverse chunk))))


(defclass skeleton-resource-handler (chunk-resource-handler) ()
  (:default-initargs :chunk-type :skeleton))


(defmethod convert-from-chunk ((this skeleton-resource-handler) chunk)
  (chunk->skeleton chunk))


(defmethod make-resource-handler ((type (eql :skeleton)) &key)
  (make-instance 'skeleton-resource-handler))
