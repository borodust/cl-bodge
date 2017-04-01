(in-package :cl-bodge.distribution)


(define-constant +system-libraries+
    (list "linux"
          "libc.so"
          "libdl.so"
          "libm.so"
          "libpthread.so"
          "librt.so"
          "libstdc++.so"
          "libxcb.so"
          "libgcc"
          "libGL"
          "libX"
          "libdrm")
  :test #'equal)


(defun list-foreign-dependencies (parent-library-path)
  (with-program-output (dep-string) ("ldd \"~a\"" (namestring parent-library-path))
    (let ((deps (split-sequence:split-sequence #\Newline dep-string)))
      (loop for dep in deps
         for div-idx = (search "=>" dep)
         for path = (trim-whitespaces (subseq dep
                                              (if div-idx (+ 2 div-idx) 0)
                                              (position #\( dep)))
         when (> (length path) 0) collect path))))


(defun system-library-p (lib-pathname)
  (let ((path (namestring lib-pathname)))
    (flet ((substringp (substring)
             (search substring path :test #'equal)))
      (or (starts-with-subseq "/lib" path)
          (some #'substringp +system-libraries+)))))


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
