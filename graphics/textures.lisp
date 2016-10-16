(in-package :cl-bodge.graphics)


(defenum texture-format
  :gray :rgb :rgba)


(defun %pixel-format->external-format (value)
  (ecase value
    (:gray :red)
    (:rgb :rgb)
    (:rgba :rgba)))


(defun %texture-format->internal-format (value)
  (ecase value
    (:gray :r8)
    (:rgb :rgb8)
    (:rgba :rgba8)))


;;;
;;;
;;;
(defclass texture (gl-object)
  ((target :initarg :target :reader target-of))
  (:default-initargs :id (gl:gen-texture)))


(define-destructor texture ((id id-of) (sys system-of))
  (-> sys
    (gl:delete-textures (list id))))


(defmacro with-texture-unit (value &body body)
  `(progn
     (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) ,value))
     ,@body))


(defmacro with-bound-texture ((place &optional (unit nil)) &body body)
  "Do not nest: rebinds to 0 after body execution and leaves new texture unit active."
  (once-only (place)
    `(unwind-protect
          (,@(if (null unit)
                 '(progn)
                 `(with-texture-unit ,unit))
             (gl:bind-texture (target-of ,place) (id-of ,place))
             ,@body)
       (gl:bind-texture (target-of ,place) 0))))


;;;
;;;
;;;
(defclass texture-2d (texture) ()
  (:default-initargs :target :texture-2d))


(defmethod initialize-instance :after ((this texture-2d) &key data external-format
                                                           internal-format width height
                                                           generate-mipmaps-p)
  (with-bound-texture (this)
    (let ((target (target-of this)))
      (gl:tex-image-2d target 0 internal-format width height 0 external-format
                       :unsigned-byte data :raw t)
      (if generate-mipmaps-p
          (gl:generate-mipmap target)
          (progn
            (gl:tex-parameter target :texture-base-level 0)
            (gl:tex-parameter target :texture-max-level 0))))))


(defun make-2d-texture (system image texture-format &optional (generate-mipmaps-p t))
  (let ((ex-format (%pixel-format->external-format (pixel-format-of image)))
        (in-format (%texture-format->internal-format texture-format)))
    (multiple-value-bind (width height) (size-of image)
      (make-instance 'texture-2d
                     :system system
                     :data (image->array image)
                     :external-format ex-format
                     :internal-format in-format
                     :generate-mipmaps-p generate-mipmaps-p
                     :width width
                     :height height))))
