(cl:in-package :cl-bodge.graphics)


(defenum texture-format
  :grey :rgb :rgba :depth :depth16 :depth-stencil)


(defun %pixel-format->external-format (value)
  (ecase value
    (:grey :red)
    (:rgb :rgb)
    (:rgba :rgba)))


(defun %texture-format->internal-format (value)
  (ecase value
    (:grey :r8)
    (:rgb :rgb8)
    (:rgba :rgba8)
    (:depth16 :depth-component16)
    (:depth :depth-component32)
    (:depth-stencil :depth24-stencil8)))


(defenum texture-wrap-mode
  :clamp-to-edge
  :clamp-to-border
  :repeat
  :mirrored-repeat)


(defclass texture-input (disposable)
  ((texture-id :initform (gl:gen-texture) :reader %texture-id-of)
   (target :initarg :target :reader %target-of)
   (dimensions :initarg :dimensions :initform nil :reader texture-dimensions)))


(define-destructor texture-input (texture-id)
  (gl:delete-texture texture-id))


(defun (setf wrap-mode-of) (mode texture)
  (with-bound-texture ((%target-of texture) (%texture-id-of texture))
    (let ((target (%target-of texture)))
      (gl:tex-parameter target :texture-wrap-s mode)
      (gl:tex-parameter target :texture-wrap-t mode)
      (gl:tex-parameter target :texture-wrap-r mode))))

;;;
;;;
;;;
(defclass texture-2d-input (texture-input) ()
  (:default-initargs :target :texture-2d))


(defmethod initialize-instance :after ((this texture-2d-input)
                                       &key image external-format
                                         internal-format width height
                                         generate-mipmaps-p)
  (with-slots (dimensions) this
    (setf dimensions (list width height)))
  (with-bound-texture ((%target-of this) (%texture-id-of this))
    (let ((target (%target-of this))
          (data (foreign-array-of image)))
      (gl:pixel-store :unpack-alignment 1)
      (gl:tex-image-2d target 0 internal-format width height 0 external-format
                       :unsigned-byte (foreign-pointer-of data) :raw t)
      (gl:tex-parameter target :texture-mag-filter :linear)
      (if generate-mipmaps-p
          (progn
            (gl:generate-mipmap target)
            (gl:tex-parameter target :texture-min-filter :nearest-mipmap-linear))
          (progn
            (gl:tex-parameter target :texture-min-filter :linear)
            (gl:tex-parameter target :texture-base-level 0)
            (gl:tex-parameter target :texture-max-level 0))))))


(define-system-function make-2d-texture graphics-system
    (image texture-format &key (generate-mipmaps-p t))
  (let ((ex-format (%pixel-format->external-format (pixel-format-of image)))
        (in-format (%texture-format->internal-format texture-format))
        (width (width-of image))
        (height (height-of image)))
      (make-instance 'texture-2d-input
                     :image image
                     :external-format ex-format
                     :internal-format in-format
                     :generate-mipmaps-p generate-mipmaps-p
                     :width width
                     :height height)))


(defmethod inject-shader-input ((this texture-2d-input) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name))
        (texture-unit (next-texture-unit)))
    (with-texture-unit (texture-unit)
      (gl:bind-texture (%target-of this) (%texture-id-of this)))
    (%gl:uniform-1i location texture-unit)))

;;;
;;;
;;;
(defclass blank-image ()
  ((width :initform nil :reader widht-of)
   (height :initform nil :reader height-of)))


(defmethod initialize-instance :after ((this blank-image) &key width height)
  (with-slots ((w width) (h height)) this
    (setf w width
          h height)))


(definline make-blank-image (width height)
  (make-instance 'blank-image :width width :height height))


(defmethod pixel-format-of ((this blank-image))
  :grey)


(defmethod foreign-array-of ((this blank-image))
  (cffi:null-pointer))
