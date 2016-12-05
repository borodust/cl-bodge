(in-package :cl-bodge.distribution)


(defclass distribution ()
  ((name :initarg :name :reader name-of)
   (target-system :initarg :target-system :reader target-system-of)
   (entry-function :initarg :entry-function :reader entry-function-of)
   (executable-name :initarg :executable-name :reader executable-name-of)
   (compressed-p :initarg :compressed-p :reader compressedp)
   (build-directory :initform nil :reader build-directory-of)
   (dist-directory :initform nil :reader directory-of)
   (library-directory :initform nil :reader library-directory-of)
   (engine-assets-directory :initform nil :reader engine-assets-directory-of)
   (assets :initform nil :reader assets-of)))


(defun path (name)
  (fad:pathname-as-directory name))


(defun file (name)
  (fad:pathname-as-file name))


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
                                                             engine-assets-directory)
  (with-slots ((this-build-dir build-directory) (this-lib-dir library-directory)
               (this-assets assets) (this-engine-assets-dir engine-assets-directory)
               target-system name dist-directory)
      this
    (let* ((sys (find-system target-system))
           (sys-path (component-pathname sys))
           (dist-name (format nil "~(~a~)" name)))
      (setf this-build-dir (if (fad:pathname-relative-p build-directory)
                               (fad:merge-pathnames-as-directory sys-path
                                                                 (path build-directory))
                               (path build-directory))
            dist-directory (fad:merge-pathnames-as-directory this-build-dir
                                                             (path dist-name))
            this-lib-dir (fad:merge-pathnames-as-directory
                          dist-directory
                          (path library-directory))
            this-engine-assets-dir (fad:merge-pathnames-as-directory
                                    dist-directory
                                    (path engine-assets-directory))
            this-assets (expand-assets-path sys-path dist-directory assets)))))


(defun parse-entry-function (entry-function)
  (etypecase entry-function
    (list
     (format nil "~(~a:~a~)" (first entry-function) (second entry-function)))
    (string
     entry-function)))


(defmacro define-distribution (name &key
                                      target-system
                                      (entry-function
                                       (error ":entry-function must be specified"))
                                      executable-name
                                      (compressed-p t)
                                      (build-directory #p"build/")
                                      (library-directory #p"lib/")
                                      (engine-assets-directory #p"assets/engine/")
                                      assets)
  `(make-instance 'distribution
                  :name ,name
                  :target-system ,(or target-system name)
                  :entry-function ,(parse-entry-function entry-function)
                  :executable-name ,(or executable-name (format nil "~(~a~).bin" name))
                  :compressed-p ,compressed-p
                  :build-directory ,build-directory
                  :library-directory ,library-directory
                  :engine-assets-directory ,engine-assets-directory
                  :assets ',assets))
