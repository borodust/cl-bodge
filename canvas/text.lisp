(cl:in-package :cl-bodge.canvas)


(defvar *active-font* nil)

(defvar *black* (vec4 0 0 0 1))

(defclass font ()
  ((face :initarg :face :reader %face-of)
   (size :initarg :size :initform nil :reader %size-of)
   (letter-spacing :initarg :letter-spacing :initform nil :reader %letter-spacing-of)
   (line-height :initarg :line-height :initform nil :reader %line-height-of)
   (alignment :initarg :alignment :initform nil :reader %alignment-of)))


(defun draw-text (position text &key (fill-color *black*) (canvas *canvas*))
  (fill-color fill-color canvas)
  (with-pushed-canvas (canvas)
    (%restore-coordinate-system canvas)
    (%nvg:text (handle-value-of canvas)
               (x position) (f (- (height-of canvas) (y position)))
               text (cffi:null-pointer))))


(defun register-font-face (name font-container &optional (canvas *canvas*))
  (let ((data (ge.rsc:font-container-data font-container)))
    (%register-font-face canvas name data)))


(defun make-font (face-id &rest args &key size letter-spacing line-height alignment)
  (declare (ignore letter-spacing line-height alignment size))
  (apply #'make-instance 'font :face face-id args))


(defun make-default-font (&rest args &key size letter-spacing line-height alignment)
  (declare (ignore letter-spacing line-height alignment size))
  (apply #'make-font nil args))


(defun %apply-font (font canvas)
  (let ((context (handle-value-of canvas)))
    (if-let ((face-id (%face-of font)))
      (%nvg:font-face-id context face-id)
      (%nvg:font-face-id context (%default-font-of canvas)))
    (when-let ((size (%size-of font)))
      (%nvg:font-size context (f size)))
    (when-let ((spacing (%letter-spacing-of font)))
      (%nvg:text-letter-spacing context (f spacing)))
    (when-let ((line-height (%line-height-of font)))
      (%nvg:text-line-height context (f line-height)))
    (when-let ((alignment (%alignment-of font)))
      (%nvg:text-align context alignment))))


(defmacro with-font ((font &optional (canvas '*canvas*)) &body body)
  (once-only (font canvas)
    `(unwind-protect
          (let ((*active-font* ,font))
            (%apply-font ,font ,canvas)
            ,@body)
       (when *active-font*
         (%apply-font *active-font* ,canvas)))))


(defun canvas-text-bounds (string &optional (canvas *canvas*))
  (static-vectors:with-static-vector (bounds 4 :element-type 'single-float)
    (let ((advance (%nvg:text-bounds (handle-value-of canvas) 0f0 0f0 string (cffi:null-pointer)
                                     (static-vectors:static-vector-pointer bounds))))
      (values (vec4 (aref bounds 0) (- (aref bounds 1)) (aref bounds 2) (- (aref bounds 3)))
              advance))))


(defun canvas-text-advance (string &optional (canvas *canvas*))
  (%nvg:text-bounds (handle-value-of canvas) 0f0 0f0 string nil nil))

;;;
;;; Metrics
;;;
(defstruct (font-metrics
            (:conc-name canvas-font-))
  (line-height (f 0) :type single-float :read-only t)
  (ascender (f 0) :type single-float :read-only t)
  (descender (f 0) :type single-float :read-only t))


(defun canvas-font-metrics (&optional (canvas *canvas*))
  (c-with ((ascender :float) (descender :float) (line-height :float))
    (%nvg:text-metrics (handle-value-of canvas) (ascender &) (descender &) (line-height &))
    (make-font-metrics :line-height line-height
                       :ascender ascender
                       :descender descender)))
