(in-package :cl-bodge.canvas)


(defgeneric font-container-data (container))


(defvar *active-font* nil)


(defvar *black* (vec4 0 0 0 1))

(defclass font ()
  ((face :initarg :face :reader %face-of)
   (size :initarg :size :reader %size-of)
   (letter-spacing :initarg :letter-spacing :initform nil :reader %letter-spacing-of)
   (line-height :initarg :line-height :initform nil :reader %line-height-of)
   (alignment :initarg :align :initform nil :reader %alignment-of)))


(defun draw-text (position text &key (fill-color *black*) (canvas *canvas*))
  (fill-color fill-color canvas)
  (with-pushed-canvas (canvas)
    (%restore-coordinate-system canvas)
    (%nvg:text (handle-value-of canvas)
               (x position) (f (- (height-of canvas) (y position)))
               text (cffi:null-pointer))))


(defun register-font-face (name font-container &optional (canvas *canvas*))
  (let ((data (font-container-data font-container)))
      (let ((font-face-id (%nvg:find-font (handle-value-of canvas) name)))
        (if (< font-face-id 0)
            (let* ((f-data (static-vectors:make-static-vector (length data) :initial-contents data))
                   (id (%nvg:create-font-mem (handle-value-of canvas) name
                                             (static-vectors:static-vector-pointer f-data)
                                             (length f-data) 0)))
              (when (< id 0)
                (static-vectors:free-static-vector f-data)
                (error "Failed to register face with name '~A'" name))
              (%register-font canvas name f-data)
              id)
            font-face-id))))


(defun make-font (face size &rest args &key letter-spacing line-height alignment)
  (declare (ignore letter-spacing line-height alignment))
  (apply #'make-instance 'font :face face :size size args))


(defun %apply-font (font canvas)
  (let ((context (handle-value-of canvas)))
    (%nvg:font-face-id context (%face-of font))
    (%nvg:font-size context (f (%size-of font)))
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
