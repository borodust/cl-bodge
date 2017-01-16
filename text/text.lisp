(in-package :cl-bodge.text)


(defclass text (disposable)
  ((text :initform nil)
   (position :initform nil)
   (text-mesh :initform nil)
   (position-buffer :initform nil)
   (texture-coord-buffer :initform nil)
   (width :initform nil :reader width-of)
   (height :initform nil :reader height-of)
   (atlas-tex :initform nil)))


(define-destructor text (text-mesh atlas-tex position-buffer texture-coord-buffer)
  (dispose texture-coord-buffer)
  (dispose position-buffer)
  (dispose text-mesh)
  (dispose atlas-tex))


(defun update-text (text string &key (start 0) end)
  (with-slots (text) text
    (setf (subseq text start (or end (length text))) string)))


(defmethod initialize-instance :after ((this text) &key text font)
  (with-slots ((this-text text) text-mesh atlas-tex width height
               position-buffer texture-coord-buffer)
      this
    (let ((pos-list (list))
          (tex-list (list)))
      (flet ((set-coords (x0 y0 x1 y1 s0 t0 s1 t1)
               (push (list x0 y0 x1 y1) pos-list)
               (push (list s0 t0 s1 t1) tex-list)))
        (multiple-value-bind (size text-width text-height)
            (walk-string text font #'set-coords)
          (setf this-text (make-array (length text)
                                      :element-type 'character
                                      :initial-contents text)
                width text-width
                height text-height
                text-mesh (make-mesh size :points)
                atlas-tex (font-atlas-texture font))
          (let ((box-array (make-array (list size 4)
                                       :element-type 'single-float
                                       :initial-contents (nreverse pos-list)))
                (tex-coord-array (make-array (list size 4)
                                             :element-type 'single-float
                                             :initial-contents (nreverse tex-list))))
            (let ((pbuf (make-array-buffer box-array))
                  (tbuf (make-array-buffer tex-coord-array)))
              (attach-array-buffer pbuf text-mesh 0)
              (attach-array-buffer tbuf text-mesh 1)
              (setf position-buffer pbuf
                    texture-coord-buffer tbuf))))))))


(defun make-text (string font)
  (make-instance 'text :text string :font font))


(defmethod render ((this text))
  (with-slots (text-mesh atlas-tex) this
    (with-bound-texture (atlas-tex)
      (render text-mesh))))
