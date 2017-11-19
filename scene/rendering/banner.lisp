(in-package :cl-bodge.scene)


(defclass banner-node (scene-node)
  ((program :initform nil)
   (texture-supplier :initarg :texture-supplier)
   ;; FIXME: share mesh too
   (mesh :initform nil)))


(defmethod discard-node :before ((this banner-node))
  (with-slots (mesh) this
    (dispose mesh)))


(defmethod initialization-flow ((this banner-node) &key (width 1.0) (height 1.0))
  (with-slots (mesh (this-program program)) this
    (>> (call-next-method)
        (-> ((graphics)) ()
          (setf mesh (make-mesh 4 :triangle-strip))
          (let ((w/2 (f (/ width 2)))
                (h/2 (f (/ height 2))))
            (with-disposable ((vbuf (make-array-buffer (make-array '(4 2)
                                                                   :element-type 'single-float
                                                                   :initial-contents
                                                                   (list (list w/2 (- h/2))
                                                                         (list w/2 h/2)
                                                                         (list (- w/2) (- h/2))
                                                                         (list (- w/2) h/2)))))
                              (tbuf (make-array-buffer #2a((1.0 0.0)
                                                           (1.0 1.0)
                                                           (0.0 0.0)
                                                           (0.0 1.0)))))
              (attach-array-buffer vbuf mesh 0)
              (attach-array-buffer tbuf mesh 1))))
        (resource-flow "/engine/shading-program/banner")
        (instantly (program)
          (setf this-program program)))))


(defmethod scene-pass ((this banner-node) (pass rendering-pass) input)
  (with-slots (texture program mesh texture-supplier) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "MVP") (model-view-projection-matrix)
            (program-uniform-variable program "banner") 0)
      (with-bound-texture ((funcall texture-supplier))
        (render mesh))))
  (call-next-method))
