(in-package :cl-bodge.asdf)


(uiop:define-package :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :local-time :alexandria :split-sequence :static-vectors)
  (:export ; from :alexandria
           with-gensyms
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
           eswitch
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
           assoc-value
           doplist)
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
           real-time-seconds
           definline
           ensure-not-null
           bound-symbol-value
           if-null
           if-bound
           when-bound
           class-name-of
           dolines
           parent
           adopt
           abandon
           abandon-all
           dochildren
           children-of
           dotree
           search-sorted
           list->array
           reexporting
           in-development-mode
           flatten-array
           expand-array
           split-sequence
           foreign-function-pointer
           stringify
           apply-argument-list
           with-float-traps-masked
           make-mutable-string
           string->mutable
           string->immutable
           mutate-string))
