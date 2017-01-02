(in-package :cl-bodge.graphics)


(defclass shader-loader (dispatcher)
  ((assets :initform (make-hash-table :test 'equal))))


(defmethod initialize-instance :after ((this shader-loader) &key)
  (with-slots (assets) this
    (loop for lib in (list-shader-libraries)
       do (setf (gethash (engine-asset-id "shader/~A" (name-of lib)) assets) lib))
    (loop for prog in (list-shading-program-descriptors)
       do (setf (gethash (engine-asset-id "program/~(~A~)" (class-name-of prog)) assets)
                prog))))


(defmethod dispatch ((this shader-loader) (task function) &rest keys &key)
  (apply #'dispatch (graphics) task keys))


(defmethod asset-names ((this shader-loader))
  (loop for key being the hash-key of (slot-value this 'assets)
       collecting key))


(defmethod load-asset (loader name)
  (with-slots (assets) loader
    (gethash name assets)))


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
