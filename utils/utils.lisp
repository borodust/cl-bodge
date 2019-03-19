(bodge-util:define-package :cl-bodge.utils
  (:nicknames :ge.util)
  (:use :cl :bodge-util)
  (:reexport :bodge-util)
  (:export #:log/debug
           #:log/trace
           #:log/info
           #:log/warn
           #:log/error
           #:log/level))
(cl:in-package :cl-bodge.utils)


(deflogger (log
            (:name cl-bodge)))
