(in-package :cl-bodge.text)


(defclass text (disposable)
  ((position :initform nil)
   (text-mesh :initform nil)
   (width :initform nil :reader width-of)
   (height :initform nil :reader height-of)
   (atlas-tex :initform nil)))


(define-destructor text (text-mesh atlas-tex)
  (dispose text-mesh)
  (dispose atlas-tex))


(defmethod initialize-instance :after ((this text) &key text font)
  (with-slots (text-mesh atlas-tex width height) this
    (let ((pos-list (list))
          (tex-list (list)))
      (flet ((set-coords (x0 y0 x1 y1 s0 t0 s1 t1)
               (push (list x0 y0 x1 y1) pos-list)
               (push (list s0 t0 s1 t1) tex-list)))
        (multiple-value-bind (size text-width text-height)
            (walk-string text font #'set-coords)
          (setf width text-width
                height text-height
                text-mesh (make-mesh size :points)
                atlas-tex (font-atlas-texture font))
          (let ((box-array (make-array (list size 4)
                                       :element-type 'single-float
                                       :initial-contents (nreverse pos-list)))
                (tex-coord-array (make-array (list size 4)
                                             :element-type 'single-float
                                             :initial-contents (nreverse tex-list))))
            (with-disposable ((pbuf (make-array-buffer box-array))
                              (tbuf (make-array-buffer tex-coord-array)))
              (attach-array-buffer pbuf text-mesh 0)
              (attach-array-buffer tbuf text-mesh 1))))))))


(defun make-text (string font)
  (make-instance 'text :text string :font font))


(defmethod render ((this text))
  (with-slots (text-mesh atlas-tex) this
    (with-bound-texture (atlas-tex)
      (render text-mesh))))
