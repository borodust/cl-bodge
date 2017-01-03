(in-package :cl-bodge.graphics)


(defclass shader-loader ()
  ((assets :initform (make-hash-table :test 'equal))))


(defun shading-program-asset-name (program-name)
  (engine-asset-id "shading-program/~(~A~)"  program-name))


(defmethod initialize-instance :after ((this shader-loader) &key)
  (with-slots (assets) this
    (loop for prog in (list-shading-program-descriptors)
       do (setf (gethash (shading-program-asset-name (class-name-of prog)) assets)
                prog))))


(defmethod asset-names ((this shader-loader))
  (loop for key being the hash-key of (slot-value this 'assets)
       collecting key))


(defmethod load-asset ((this shader-loader) name)
  (with-slots (assets) this
    (-> ((graphics) :important-p t) ()
      (load-shading-program (gethash name assets)))))


(defmethod release-asset ((this shader-loader) asset-name)
  (with-slots (assets) this
    (if-let ((asset (gethash asset-name assets)))
      (etypecase asset
        (shader-library (clear-library-cache asset))
        (shading-program-descriptor (clear-cached-program asset)))
      (warn "Asset with name ~A not found in loader ~A" asset-name this))))


(defmethod path-to (asset))


(define-system-function make-shader-loader graphics-system ()
  (make-instance 'shader-loader))
