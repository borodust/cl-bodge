(in-package :cl-bodge.poiu)


(defstruct (text-cache
             (:constructor make-text-cache (font)))
  (font nil :read-only t)
  (table (make-hash-table :test 'equal) :read-only t))



(definline get-text (text-cache string)
  (if-let ((text (gethash string (text-cache-table text-cache))))
    text
    (setf (gethash string (text-cache-table text-cache))
          (make-text string (text-cache-font text-cache)))))


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
                     (translation-mat4 #f(- (/ width 2)) #f(- (/ height 2)) 0.0)))))


(define-system-function make-text-renderer graphics-system
    (width height font line-height &key (color (vec4 0.0 0.0 0.0 1.0)))
  (make-instance 'text-renderer
                 :shading-program (load-shading-program 'text-shading)
                 :font font
                 :width width
                 :height height
                 :line-height line-height
                 :default-color color))


(defun render-text (renderer string &key position color)
  (with-slots (text-cache shading-program proj default-color height scale) renderer
    (let* ((text (get-text text-cache string))
           (font (text-cache-font text-cache))
           (model-view-mat (if position
                             (mult proj
                                   (translation-mat4 (x position)
                                                     (- height (y position)
                                                        (* scale (font-ascender-height font)))
                                                     -1.0))
                             proj)))
      (with-using-shading-program (shading-program)
        (setf (program-uniform-variable shading-program "atlas") 0
              (program-uniform-variable shading-program "scale") scale
              (program-uniform-variable shading-program "baseColor") (or color default-color)
              (program-uniform-variable shading-program "proj") model-view-mat)
        (render text)))))
