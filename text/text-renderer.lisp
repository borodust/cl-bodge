(cl:in-package :cl-bodge.text)


(defclass text-renderer (disposable)
  ((default-color :initarg :default-color)
   (pipeline :initarg :pipeline)
   (scale :initform 1.0 :reader scale-of)
   (line-height :initarg :line-height :reader text-line-height)
   (proj :initform nil)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (text-cache :initform nil)))


(define-destructor text-renderer (pipeline)
  (dispose pipeline))


(defgeneric font-of (obj)
  (:method ((obj text-renderer))
    (with-slots (text-cache) obj
      (text-cache-font text-cache))))


(defun update-text-renderer-canvas-size (text-renderer width height)
  (with-slots (proj) text-renderer
    (setf proj (mult (orthographic-projection-mat width height 0.0 1.0)
                     (translation-mat4 (- (/ width 2)) (- (/ height 2)) 0.0)))))


(defmethod initialize-instance :after ((this text-renderer) &key font width height line-height)
  (with-slots (text-cache scale) this
    (setf text-cache (make-text-cache font)
          scale (/ line-height (+ (font-ascender-height font)
                                  (font-descender-height font)
                                  (font-line-gap font)))))
  (update-text-renderer-canvas-size this width height))


(define-system-function make-text-renderer graphics-system
    (width height font line-height &key (color (vec4 0.0 0.0 0.0 1.0)))
  (make-instance 'text-renderer
                 :pipeline (make-pipeline 'text-pipeline)
                 :font font
                 :width width
                 :height height
                 :line-height line-height
                 :default-color color))


(defun text-ascender-height (text-renderer)
  (* (scale-of text-renderer) (font-ascender-height (font-of text-renderer))))


(defun measure-scaled-string (renderer string)
  (with-slots (text-cache scale) renderer
    (flet ((scale (v)
             (* v scale)))
      (mapcar #'scale (measure-string string (text-cache-font text-cache))))))


(defun print-text (renderer string &key position color)
  (with-slots (text-cache pipeline proj default-color scale) renderer
    (let* ((text (get-text text-cache string))
           (model-view-mat (if position
                             (mult proj (translation-mat4 (x position)
                                                          (y position)
                                                          0.0))
                             proj)))
      (render-text t pipeline text :scale (f scale)
                                   :base-color (or color default-color)
                                   :mvp-matrix model-view-mat))))
