(in-package :cl-bodge.poiu)


(defclass text-renderer ()
  ((default-color :initarg :default-color)
   (shading-program :initarg :shading-program)
   (scale :initform 1.0 :reader scale-of)
   (proj :initform nil)
   (width :initarg :width :reader width-of)
   (height :initarg :height :reader height-of)
   (text-cache :initform nil)))


(defun text-renderer-font (obj)
  (with-slots (text-cache) obj
    (text-cache-font text-cache)))


(defmethod initialize-instance :after ((this text-renderer) &key font width height line-height)
  (with-slots (text-cache proj scale) this
    (setf text-cache (make-text-cache font)
          scale (/ line-height (+ (font-ascender-height font)
                                  (font-descender-height font)
                                  (font-line-gap font)))
          proj (mult (orthographic-projection-mat width height -1.0 1.0)
                     (translation-mat4 #f(- (/ width 2)) #f(- (/ height 2)) -1.0)))))


(define-system-function make-text-renderer graphics-system
    (width height font line-height &key (color (vec4 0.0 0.0 0.0 1.0)))
  (make-instance 'text-renderer
                 :shading-program (load-shading-program
                                   (find-program-descriptor 'text-shading))
                 :font font
                 :width width
                 :height height
                 :line-height line-height
                 :default-color color))


(defun render-cached-text (renderer string &key position color)
  (with-slots (text-cache shading-program proj default-color height scale) renderer
    (let* ((text (get-text text-cache string))
           (font (text-cache-font text-cache))
           (model-view-mat (if position
                             (mult proj
                                   (translation-mat4 (x position)
                                                     (- height (y position)
                                                        (* scale (font-ascender-height font)))
                                                    0.0))
                             proj)))
      (with-active-shading-program (shading-program)
        (setf (program-uniform-variable shading-program "atlas") 0
              (program-uniform-variable shading-program "scale") scale
              (program-uniform-variable shading-program "baseColor") (or color default-color)
              (program-uniform-variable shading-program "proj") model-view-mat)
        (render text)))))
