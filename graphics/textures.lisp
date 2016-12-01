(in-package :cl-bodge.graphics)


(declaim (special *active-texture*
                  *active-texture-unit*))


(defenum texture-format
  :grey :rgb :rgba)


(defun %pixel-format->external-format (value)
  (ecase value
    (:grey :red)
    (:rgb :rgb)
    (:rgba :rgba)))


(defun %texture-format->internal-format (value)
  (ecase value
    (:grey :r8)
    (:rgb :rgb8)
    (:rgba :rgba8)))


(defenum texture-wrap-mode
  :clamp-to-edge
  :clamp-to-border
  :repeat
  :mirrored-repeat)


;;;
;;;
;;;
(defclass texture (gl-object)
  ((target :initarg :target :reader target-of))
  (:default-initargs :id (gl:gen-texture)))


(define-destructor texture ((id id-of) (sys system-of))
  (-> (sys)
    (gl:delete-textures (list id))))


(defun use-texture-unit (val)
  (gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) val)))


(defmacro with-texture-unit (value &body body)
  (once-only (value)
    `(unwind-protect
          (progn
            (use-texture-unit ,value)
            (let ((*active-texture-unit* ,value))
                  ,@body))
       (if-bound *active-texture-unit*
                 (use-texture-unit *active-texture-unit*)
                 (use-texture-unit ,value)))))


(defmacro with-bound-texture ((place &optional (unit nil)) &body body)
  (once-only (place)
    `(unwind-protect
          (,@(if (null unit)
                 '(progn)
                 `(with-texture-unit ,unit))
             (gl:bind-texture (target-of ,place) (id-of ,place))
             (let ((*active-texture* ,place))
               ,@body))
       (if-bound *active-texture*
                 (gl:bind-texture (target-of *active-texture*) (id-of *active-texture*))
                 (gl:bind-texture (target-of ,place) 0)))))


(defun (setf wrap-mode-of) (mode texture)
  (with-bound-texture (texture)
    (let ((target (target-of texture)))
      (gl:tex-parameter target :texture-wrap-s mode)
      (gl:tex-parameter target :texture-wrap-t mode)
      (gl:tex-parameter target :texture-wrap-r mode))))



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
      (gl:pixel-store :unpack-alignment 1)
      (gl:tex-image-2d target 0 internal-format width height 0 external-format
                       :unsigned-byte data :raw t)
      (if generate-mipmaps-p
          (gl:generate-mipmap target)
          (progn
            (gl:tex-parameter target :texture-base-level 0)
            (gl:tex-parameter target :texture-max-level 0))))))


(define-system-function make-2d-texture graphics-system
    (image texture-format &key (generate-mipmaps-p t) (system *system*))
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
