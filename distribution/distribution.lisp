(in-package :cl-bodge.distribution)


(defclass distribution ()
  ((name :initarg :name :reader name-of)
   (target-system :initarg :target-system :reader target-system-of)
   (entry-function :initarg :entry-function :reader entry-function-of)
   (executable-name :initarg :executable-name :reader executable-name-of)
   (configuration-file :initarg :configuration-file :reader configuration-file-of)
   (compressed-p :initarg :compressed-p :reader compressedp)
   (build-directory :initform nil :reader build-directory-of)
   (dist-directory :initform nil :reader directory-of)
   (library-directory :initform nil :reader library-directory-of)
   (assets-directory :initform nil :reader assets-directory-of)
   (assets :initform nil :reader assets-of)
   (bundle-name :initarg :bundle-name :reader bundle-name-of)
   (bundle-run-file :initarg :bundle-run-file :reader bundle-run-file-of)
   (bundle-compressed-p :initarg :bundle-compressed-p :reader bundle-compressed-p)))


(defun expand-assets-path (src dst assets)
  (flet ((translated (asset)
           (destructuring-bind (asset-src asset-dst) (if (listp asset) asset (list asset asset))
             (let ((asset-src-absolute (fad:merge-pathnames-as-file src (file asset-src))))
               (if-let ((path (fad:file-exists-p asset-src-absolute)))
                 (if (fad:directory-pathname-p path)
                     (list path (fad:merge-pathnames-as-directory dst (path asset-dst)))
                     (list path (fad:merge-pathnames-as-file dst (file asset-dst))))
                 (error "File or directory '~a' not found" asset-src-absolute))))))
    (loop for asset in assets collecting (translated asset))))


(defmethod initialize-instance :after ((this distribution) &key build-directory
                                                             library-directory
                                                             assets
                                                             configuration-file
                                                             assets-directory
                                                             base-directory)
  (with-slots ((this-build-dir build-directory) (this-lib-dir library-directory)
               (this-assets assets) (this-assets-dir assets-directory)
               target-system name dist-directory (this-configuration-file configuration-file))
      this
    (let* ((sys (find-system target-system))
           (base-path (fad:merge-pathnames-as-directory (component-pathname sys)
                                                        base-directory))
           (dist-name (format nil "~(~a~)" name)))
      (setf this-build-dir (if (fad:pathname-relative-p build-directory)
                               (fad:merge-pathnames-as-directory base-path
                                                                 (path build-directory))
                               (path build-directory))
            dist-directory (fad:merge-pathnames-as-directory this-build-dir
                                                             (path dist-name))
            this-lib-dir (fad:merge-pathnames-as-directory
                          dist-directory
                          (path library-directory))
            this-configuration-file (when configuration-file (file base-path configuration-file))
            this-assets-dir (path assets-directory)
            this-assets (expand-assets-path base-path dist-directory assets)))))


(defun parse-entry-function (entry-function)
  (etypecase entry-function
    (list
     (format nil "~(~A::~A~)" (first entry-function) (second entry-function)))
    (string
     entry-function)))


(defmacro descriptor (name &body body
                        &key target-system
                          (entry-function
                           (error ":entry-function must be specified"))
                          (base-directory "./")
                          executable-name
                          (compressed-p t)
                          (build-directory #p"build/")
                          (library-directory #p"lib/")
                          configuration-file
                          (assets-directory #p"assets/")
                          assets
                          bundle)
  (declare (ignore body))
  (destructuring-bind (&key ((:name bundle-name) (format nil "~(~a~)" name))
                            ((:run-file bundle-run-file))
                            ((:compressed-p bundle-compressed-p) t))
      bundle
    (when (and bundle (null bundle-run-file))
      (error ":run-file must be specified for the bundle"))
    (once-only (name)
      `(register-distribution
        ,name
        (make-instance 'distribution
                       :name ,name
                       :base-directory ,base-directory
                       :target-system ,(or target-system name)
                       :entry-function ,(parse-entry-function entry-function)
                       :executable-name ,(or executable-name `(format nil "~(~a~).bin" ,name))
                       :compressed-p ,compressed-p
                       :build-directory ,build-directory
                       :library-directory ,library-directory
                       :assets-directory ,assets-directory
                       :configuration-file ,configuration-file
                       :assets ',assets
                       :bundle-name ,bundle-name
                       :bundle-compressed-p ,bundle-compressed-p
                       :bundle-run-file ,bundle-run-file)))))
