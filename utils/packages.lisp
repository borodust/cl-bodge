(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :local-time :alexandria :split-sequence)
  ;; reexport from alexandria
  (:export with-gensyms
           once-only
           symbolicate
           make-keyword
           format-symbol
           parse-body
           parse-ordinary-lambda-list
           when-let
           when-let*
           if-let
           switch
           define-constant
           alist-hash-table
           plist-alist
           read-file-into-string
           nconcf
           starts-with-subseq
           positive-integer
           copy-array
           deletef
           alist-hash-table
           ensure-list
           assoc-value)
  (:import-from :uiop
                define-package)
  (:export define-package)
  (:export log-errors
           with-hash-entries
           make-hash-table-with-entries
           stream->byte-array
           file->byte-array
           defenum
           f
           epoch-seconds
           definline
           ensure-not-null
           bound-symbol-value
           if-bound
           when-bound
           class-name-of
           dolines
           parent
           adopt
           abandon
           dochildren
           children-of
           dotree
           search-sorted
           list->array
           reexporting
           when-debugging
           flatten-array
           split-sequence))
