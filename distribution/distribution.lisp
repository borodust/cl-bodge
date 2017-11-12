(in-package :cl-bodge.distribution)

(defvar *build-directory* nil)


(defclass distribution ()
  ((name :initarg :name :reader name-of)
   (base-directory :initarg :base-directory :reader base-directory-of)
   (target-system :initarg :target-system :reader target-system-of)
   (entry-function :initarg :entry-function :reader entry-function-of)
   (executable-name :initarg :executable-name :reader executable-name-of)
   (configuration-file :initarg :configuration-file)
   (compressed-p :initarg :compressed-p :reader compressedp)
   (build-directory :initarg :build-directory)
   (library-directory :initarg :library-directory)
   (asset-directory :initarg :asset-directory)
   (prologue :initform nil :initarg :prologue)
   (epilogue :initform nil :initarg :epilogue)
   (resources :initform nil :initarg :resources)
   (asset-containers :initform nil :initarg :asset-containers :reader asset-containers-of)
   (bindings :initform nil :initarg :bindings :reader bindings-of)
   (bundle-name :initarg :bundle-name :reader bundle-name-of)
   (bundle-run-file :initarg :bundle-run-file :reader bundle-run-file-of)
   (bundle-compressed-p :initarg :bundle-compressed-p :reader bundle-compressed-p)))


(defun expand-container-paths (dst containers)
  (flet ((translated (container)
           (destructuring-bind (resource-path container-path) container
             (cons resource-path (fad:merge-pathnames-as-file dst container-path)))))
    (loop for container in containers collecting (translated container))))


(defgeneric build-directory-of (distribution)
  (:method ((this distribution))
    (with-slots (build-directory) this
      (dir (base-directory-of this) (or *build-directory* build-directory)))))


(defgeneric directory-of (distribution)
  (:method ((this distribution))
    (let ((dist-name (format nil "~(~a~)" (name-of this))))
      (dir (build-directory-of this) dist-name))))


(defgeneric library-directory-of (distribution)
  (:method ((this distribution))
    (with-slots (library-directory) this
      (dir (directory-of this) library-directory))))


(defgeneric configuration-file-of (distribution)
  (:method ((this distribution))
    (with-slots (configuration-file) this
      (when configuration-file
        (file (base-directory-of this) configuration-file)))))


(defgeneric asset-directory-of (distribution)
  (:method ((this distribution))
    (with-slots (asset-directory) this
      (dir (directory-of this) asset-directory))))


(defgeneric prologue-of (distribution)
  (:method ((this distribution))
    (with-slots (prologue) this
      (when prologue
        (file (base-directory-of this) prologue)))))


(defgeneric epilogue-of (distribution)
  (:method ((this distribution))
    (with-slots (epilogue) this
      (when epilogue
        (file (base-directory-of this) epilogue)))))


(defun expand-resource-paths (src dst assets)
  (flet ((translated (asset)
           (destructuring-bind (asset-src asset-dst) (if (listp asset) asset (list asset asset))
             (let ((asset-src-absolute (file src asset-src)))
               (if-let ((path (fad:file-exists-p asset-src-absolute)))
                 (if (fad:directory-pathname-p path)
                     (list path (dir dst asset-dst))
                     (list path (file dst  asset-dst)))
                 (error "File or directory '~a' not found" asset-src-absolute))))))
    (loop for asset in assets collecting (translated asset))))


(defgeneric resources-of (distribution)
  (:method ((this distribution))
    (with-slots (resources) this
      (expand-resource-paths (base-directory-of this) (directory-of this) resources))))


(defmethod initialize-instance :after ((this distribution) &key base-directory)
  (with-slots ((this-base-dir base-directory)
               target-system name)
      this
    (let* ((sys (find-system target-system)))
      (setf this-base-dir (dir (component-pathname sys) base-directory)))))


(defun parse-entry-function (entry-function)
  (etypecase entry-function
    (list
     (format nil "~(~A::~A~)" (first entry-function) (second entry-function)))
    (string
     entry-function)))


(defun expand-bindings (list)
  `(list ,@(loop for (symbol value) in list
              collect `(cons ',symbol ,value))))


(defun register-distribution (name entry-function
                              &key target-system
                                base-directory
                                executable-name
                                (compressed-p t)
                                build-directory
                                library-directory
                                configuration-file
                                prologue
                                epilogue
                                bindings
                                asset-directory
                                resources
                                asset-containers
                                bundle-name
                                bundle-compressed-p
                                bundle-run-file)
  (unless (and name entry-function)
    (error "name and entry-function must be specified"))
  (when (and bundle-name (null bundle-run-file))
    (error "bundle-run-file must be specified for the bundle if name is provided"))
  (let* ((base-directory (or base-directory "./"))
         (build-directory (or build-directory  #p"build/"))
         (library-directory (or library-directory #p"lib/"))
         (asset-directory (or asset-directory #p"assets/"))
         (dist (make-instance 'distribution
                              :name name
                              :base-directory base-directory
                              :target-system (or target-system name)
                              :entry-function (parse-entry-function entry-function)
                              :executable-name (or executable-name (format nil "~(~a~)" name))
                              :compressed-p compressed-p
                              :build-directory build-directory
                              :library-directory library-directory
                              :asset-directory asset-directory
                              :configuration-file configuration-file
                              :epilogue epilogue
                              :prologue prologue
                              :bindings bindings
                              :resources resources
                              :asset-containers asset-containers
                              :bundle-name bundle-name
                              :bundle-compressed-p bundle-compressed-p
                              :bundle-run-file bundle-run-file)))
    (%register-distribution name dist)))


(defmacro descriptor (name &body body
                        &key target-system
                          entry-function
                          base-directory
                          executable-name
                          (compressed-p t)
                          build-directory
                          library-directory
                          configuration-file
                          prologue
                          epilogue
                          bind
                          asset-directory
                          resources
                          asset-containers
                          bundle)
  (declare (ignore body))
  (destructuring-bind (&key ((:name bundle-name))
                            ((:run-file bundle-run-file))
                            ((:compressed-p bundle-compressed-p) t))
      bundle
    (once-only (name)
      `(register-distribution ,name ',entry-function
                              :base-directory ,base-directory
                              :target-system ,target-system
                              :executable-name ,executable-name
                              :compressed-p ,compressed-p
                              :build-directory ,build-directory
                              :library-directory ,library-directory
                              :asset-directory ,asset-directory
                              :configuration-file ,configuration-file
                              :epilogue ,epilogue
                              :prologue ,prologue
                              :bindings ,(expand-bindings bind)
                              :resources ',resources
                              :asset-containers ',asset-containers
                              :bundle-name ,bundle-name
                              :bundle-compressed-p ,bundle-compressed-p
                              :bundle-run-file ,bundle-run-file))))
