(in-package :cl-bodge.distribution)


(define-constant +system-libraries+
    (list "linux-[\\w_-]+\\.so"
          "libc[.]so"
          "libdl[.]so"
          "libm[.]so"
          "libpthread[.]so"
          "librt[.]so"
          "libxcb[.]so"
          "libgcc.*[.]so"
          "libGL.*[.]so"
          "libX.*[.]so"
          "libdrm.*[.]so")
  :test #'equal)


(defun list-foreign-dependencies (parent-library-path)
  (find-dependencies-with-ldd parent-library-path))


(defun system-library-p (lib-pathname)
  (let ((path (namestring lib-pathname)))
    (flet ((matchesp (regex)
             (ppcre:scan regex path)))
      (some #'matchesp +system-libraries+))))


(defun list-platform-search-paths ()
  (let (result)
    (labels ((walker (path)
               (if (fad:directory-pathname-p path)
                   (fad:walk-directory path #'walker)
                   (let ((lines (split-sequence:split-sequence #\Newline
                                                               (read-file-into-string path))))
                     (loop for line in lines
                        for trimmed = (trim-whitespaces line)
                        when (> (length trimmed) 0) do
                          (unless (starts-with #\# line)
                            (push (fad:pathname-as-directory trimmed) result)))))))
      (fad:walk-directory "/etc/ld.so.conf.d/" #'walker))
    result))


(defun make-app-bundle ())
