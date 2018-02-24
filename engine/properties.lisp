(cl:in-package :cl-bodge.engine)


(defun %get-property (key properties &optional (default-value nil))
  (if (listp key)
      (if-let ((rest-keys (rest key)))
        (%get-property rest-keys (getf properties (first key)) default-value)
        (%get-property (first key) properties default-value))
      (getf properties key default-value)))


(defun %load-properties (property-source)
  (etypecase property-source
    (list (if (consp (first property-source))
              (alist-plist property-source)
              property-source))
    (hash-table (hash-table-plist property-source))
    ((or string pathname)
     (with-open-file (stream (fad:canonical-pathname property-source))
       (let ((form (read stream nil nil)))
         (%load-properties (if (eq 'quote (first form))
                               (second form)
                               (first form))))))))
