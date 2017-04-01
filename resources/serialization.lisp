(in-package :cl-bodge.resources)


(defgeneric encode-resource (asset stream)
  (:method (asset stream)))


(defgeneric decode-resource (class stream)
  (:method (class stream)))
