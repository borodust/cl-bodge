(cl:in-package :cl-bodge.graphics)


(defenum texture-format
  :grey :rgb :rgba :depth :depth16 :depth24 :depth-stencil)


(defun %pixel-format->external-format (value)
  (ecase value
    (:grey :red)
    (:rgb :rgb)
    (:rgba :rgba)
    (:depth :depth-component)))


(defun %texture-format->internal-format (value)
  (ecase value
    (:grey :r8)
    (:rgb :rgb8)
    (:rgba :rgba8)
    (:depth16 :depth-component16)
    (:depth24 :depth-component24)
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
   (format :initarg :internal-format :reader texture-format)
   (max-level :initform 1000 :accessor %texture-max-level-of)
   (dimensions :initarg :dimensions :initform nil :reader texture-dimensions)))


(define-destructor texture-input (texture-id)
  (dispose-gl-object texture-id #'gl:delete-texture))


(defun (setf wrap-mode-of) (mode texture)
  (with-bound-texture ((%target-of texture) (%texture-id-of texture))
    (let ((target (%target-of texture)))
      (gl:tex-parameter target :texture-wrap-s mode)
      (gl:tex-parameter target :texture-wrap-t mode)
      (gl:tex-parameter target :texture-wrap-r mode))))


(defmethod inject-shader-input ((this texture-input) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name))
        (texture-unit (next-texture-unit)))
    (with-texture-unit (texture-unit)
      (gl:bind-texture (%target-of this) (%texture-id-of this)))
    (%gl:uniform-1i location texture-unit)))


(defgeneric assign-texture-mipmap-level (image texture level))


(defun texture-mipmap-level (texture level)
  (declare (ignore texture level))
  (error "Unimplemented"))


(defun (setf texture-mipmap-level) (image texture level)
  (assign-texture-mipmap-level image texture level))

;;;
;;; 2D TEXTURE
;;;
(defclass texture-2d (texture-input)
  ()
  (:default-initargs :target :texture-2d))


(defmethod initialize-instance :after ((this texture-2d)
                                       &key image external-format
                                         internal-format width height
                                         generate-mipmaps-p)
  (with-slots (dimensions) this
    (setf dimensions (list width height)))
  (with-bound-texture ((%target-of this) (%texture-id-of this))
    (let ((target (%target-of this))
          (data (ge.rsc:image->foreign-array image)))
      (gl:pixel-store :unpack-alignment 1)
      (gl:tex-image-2d target 0 internal-format width height 0 external-format
                       :unsigned-byte (foreign-pointer-of data) :raw t)
      (gl:tex-parameter target :texture-mag-filter :linear)
      (gl:tex-parameter target :texture-base-level 0)
      (if generate-mipmaps-p
          (progn
            (gl:tex-parameter target :texture-max-level (%texture-max-level-of this))
            (gl:generate-mipmap target)
            (gl:tex-parameter target :texture-min-filter :nearest-mipmap-linear))
          (progn
            (gl:tex-parameter target :texture-min-filter :linear)
            (gl:tex-parameter target :texture-max-level (setf (%texture-max-level-of this) 0)))))))


(defun %make-2d-texture (class image texture-format &key (generate-mipmaps-p t))
  (let ((ex-format (%pixel-format->external-format (ge.rsc:image-pixel-format image)))
        (in-format (%texture-format->internal-format texture-format))
        (width (ge.rsc:image-width image))
        (height (ge.rsc:image-height image)))
    (make-instance class
                   :image image
                   :external-format ex-format
                   :internal-format in-format
                   :generate-mipmaps-p generate-mipmaps-p
                   :width width
                   :height height)))


(define-system-function make-2d-texture graphics-system
    (image texture-format &key (generate-mipmaps-p t))
  (%make-2d-texture 'texture-2d image texture-format
                    :generate-mipmaps-p generate-mipmaps-p))


(defun %assign-texture-mipmap-level (image texture level-target level)
  (let ((target (%target-of texture))
        (external-format (%pixel-format->external-format (image-pixel-format image)))
        (width (image-width image))
        (height (image-height image))
        (data (image->foreign-array image)))
    (with-bound-texture (target (%texture-id-of texture))
      (gl:tex-image-2d level-target level (texture-format texture) width height 0 external-format
                       :unsigned-byte (foreign-pointer-of data) :raw t)
      (when (> level (%texture-max-level-of texture))
        (gl:tex-parameter target :texture-max-level (setf (%texture-max-level-of texture) level))))))


(defmethod assign-texture-mipmap-level (image (texture texture-2d) level)
  (%assign-texture-mipmap-level image texture (%target-of texture) level))


;;;
;;; CUBEMAP TEXTURE
;;;
(defclass cubemap-texture (texture-input) ()
  (:default-initargs :target :texture-cube-map))


(defun %make-cubemap-texture (class
                              positive-x-image
                              positive-y-image
                              positive-z-image
                              negative-x-image
                              negative-y-image
                              negative-z-image
                              texture-format &key (generate-mipmaps-p t))
  (let ((ex-format (%pixel-format->external-format (image-pixel-format positive-x-image)))
        (in-format (%texture-format->internal-format texture-format))
        (width (image-width positive-x-image))
        (height (image-height positive-x-image)))
    (unless (= width height)
      (error "Cubemap texture must have same width and height"))
    (make-instance class
                   :images (list :texture-cube-map-positive-x positive-x-image
                                 :texture-cube-map-positive-y positive-y-image
                                 :texture-cube-map-positive-z positive-z-image
                                 :texture-cube-map-negative-x negative-x-image
                                 :texture-cube-map-negative-y negative-y-image
                                 :texture-cube-map-negative-z negative-z-image)
                   :external-format ex-format
                   :internal-format in-format
                   :generate-mipmaps-p generate-mipmaps-p
                   :size width)))


(defmethod initialize-instance :after ((this cubemap-texture)
                                       &key images
                                         external-format
                                         internal-format
                                         generate-mipmaps-p
                                         size)
  (with-slots (dimensions) this
    (setf dimensions (list size size)))
  (with-bound-texture ((%target-of this) (%texture-id-of this))
    (loop for (target image) on images by #'cddr
          do (let ((data (image->foreign-array image)))
               (unless (and (= (image-width image) size) (= (image-height image) size))
                 (error "Cubemap images all must have the same edge size"))
               (gl:tex-image-2d target 0 internal-format size size 0 external-format
                                :unsigned-byte (foreign-pointer-of data) :raw t)))
    (let ((target (%target-of this)))
      (gl:tex-parameter target :texture-mag-filter :linear)
      (if generate-mipmaps-p
          (progn
            (gl:generate-mipmap target)
            (gl:tex-parameter target :texture-min-filter :nearest-mipmap-linear))
          (progn
            (gl:tex-parameter target :texture-min-filter :linear)
            (gl:tex-parameter target :texture-base-level 0)
            (gl:tex-parameter target :texture-max-level 0))))))


(defun make-cubemap-texture (positive-x-image
                             positive-y-image
                             positive-z-image
                             negative-x-image
                             negative-y-image
                             negative-z-image
                             texture-format
                             &key (generate-mipmaps-p t))
  (%make-cubemap-texture 'cubemap-texture positive-x-image
                         positive-y-image
                         positive-z-image
                         negative-x-image
                         negative-y-image
                         negative-z-image
                         texture-format
                         :generate-mipmaps-p generate-mipmaps-p))


(defgeneric cubemap-positive-x-layer (cubemap))
(defgeneric cubemap-positive-y-layer (cubemap))
(defgeneric cubemap-positive-z-layer (cubemap))
(defgeneric cubemap-negative-x-layer (cubemap))
(defgeneric cubemap-negative-y-layer (cubemap))
(defgeneric cubemap-negative-z-layer (cubemap))


(defclass cubemap-texture-layer ()
  ((texture :initarg :texture :reader %texture-of)
   (layer-id :initarg :layer-id :reader %layer-type-of)))


(defmethod cubemap-positive-x-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-x))

(defmethod cubemap-positive-y-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-y))

(defmethod cubemap-positive-z-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-z))

(defmethod cubemap-negative-x-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-x))

(defmethod cubemap-negative-y-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-y))

(defmethod cubemap-negative-z-layer ((this cubemap-texture))
  (make-instance 'cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-z))


(defmethod assign-texture-mipmap-level (image (layer cubemap-texture-layer) level)
  (%assign-texture-mipmap-level image (%texture-of layer) (%layer-type-of layer) level))

;;;
;;;
;;;
(defclass blank-image ()
  ((width :initform nil :reader image-width)
   (height :initform nil :reader image-height)
   (pixel-format :initform :grey :initarg :pixel-format :reader image-pixel-format)))


(defmethod initialize-instance :after ((this blank-image) &key width height)
  (with-slots ((w width) (h height)) this
    (setf w width
          h height)))


(definline make-blank-image (width height &key (format :grey))
  (make-instance 'blank-image :width width :height height :pixel-format format))


(defmethod image->foreign-array ((this blank-image))
  nil)


(define-system-function make-empty-2d-texture graphics-system (width height texture-format)
  (make-2d-texture (make-blank-image width height) texture-format :generate-mipmaps-p nil))


(define-system-function make-empty-cubemap-texture graphics-system (width texture-format)
  (let ((blank-image (make-blank-image width width)))
    (make-cubemap-texture blank-image
                          blank-image
                          blank-image
                          blank-image
                          blank-image
                          blank-image
                          texture-format :generate-mipmaps-p nil)))


(defclass depth-texture (texture-2d) ())


(define-system-function make-empty-depth-texture graphics-system (width height)
  (%make-2d-texture 'depth-texture (make-blank-image width height :format :depth)
                    :depth :generate-mipmaps-p nil))


(defclass depth-cubemap-texture-layer (cubemap-texture-layer) ())


(defclass depth-cubemap-texture (cubemap-texture) ())


(define-system-function make-empty-depth-cubemap-texture graphics-system (edge-size)
  (let ((blank-image (make-blank-image edge-size edge-size :format :depth)))
    (%make-cubemap-texture 'depth-cubemap-texture
                           blank-image
                           blank-image
                           blank-image
                           blank-image
                           blank-image
                           blank-image
                           :depth :generate-mipmaps-p nil)))


(defmethod cubemap-positive-x-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-x))

(defmethod cubemap-positive-y-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-y))

(defmethod cubemap-positive-z-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-positive-z))

(defmethod cubemap-negative-x-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-x))

(defmethod cubemap-negative-y-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-y))

(defmethod cubemap-negative-z-layer ((this depth-cubemap-texture))
  (make-instance 'depth-cubemap-texture-layer
                 :texture this
                 :layer-id :texture-cube-map-negative-z))
