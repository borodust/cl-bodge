(in-package :cl-bodge.scene)


(declaim (special *banner-texture*))


(defclass banner-node (scene-node)
  ((program :initarg :program)
   ;; FIXME: share mesh too
   (mesh :initform nil)))


(defmethod discard-node banner :before ((this banner-node))
  (with-slots (mesh) this
    (dispose mesh)))


(defmethod assemble-node ((class (eql 'banner-node)) &key)
  (>> (call-next-method)
      (-> ((graphics)) (this)
        (with-slots (mesh) this
          (setf mesh (make-mesh 4 :triangle-strip))
          (with-disposable ((vbuf (make-array-buffer (make-array '(4 2)
                                                                 :element-type 'single-float
                                                                 :initial-contents
                                                                 '((1.0 0.0)
                                                                   (1.0 1.0)
                                                                   (0.0 0.0)
                                                                   (0.0 1.0)))))
                            (tbuf (make-array-buffer #2a((1.0 0.0)
                                                         (1.0 1.0)
                                                         (0.0 0.0)
                                                         (0.0 1.0)))))
            (attach-array-buffer vbuf mesh 0)
            (attach-array-buffer tbuf mesh 1)))
        this)))


(defmethod scene-pass ((this banner-node) (pass rendering-pass) input)
  (with-slots (texture program mesh) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "MVP") (model-view-projection-matrix)
            (program-uniform-variable program "banner") 0)
      (with-bound-texture (*banner-texture*)
        (render mesh))))
  (call-next-method))
