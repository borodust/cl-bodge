(in-package :cl-bodge.engine)


(defun %get-property (key properties &optional (default-value nil))
  (if-let ((property (assoc key properties :test #'equal)))
    (cdr property)
    (if (functionp default-value)
        (funcall default-value)
	default-value)))


(defun %load-properties (path)
  (with-open-file (stream (fad:canonical-pathname path))
    (loop for form = (read stream nil nil) while form with properties = '() do
         (destructuring-bind (key value) form
           (setf properties (acons key value properties)))
       finally (return properties))))
