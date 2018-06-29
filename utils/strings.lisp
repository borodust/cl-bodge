(cl:in-package :cl-bodge.utils)


(defun for-each-line (text action)
  (when text
    (loop with line-start = 0 and slen = (length text)
          for line-end = (or (position #\Newline text :start line-start) slen)
          for line = (make-array (- line-end line-start) :element-type (array-element-type text)
                                                         :displaced-to text
                                                         :displaced-index-offset line-start)
          do (funcall action line)
             (setf line-start (1+ line-end))
          until (= line-end slen))))


(defmacro dolines ((line-var text &optional result-form) &body body)
  `(block nil
     (for-each-line ,text (lambda (,line-var) ,@body))
     ,result-form))


(defun stringify (value &optional (format-string "~A"))
  (if (stringp value)
      value
      (format nil format-string value)))



;;;
;;; Mutable string
;;;
(defun make-mutable-string (&optional (length 0))
  (make-array length :element-type 'character :fill-pointer t))


(defun string->mutable (string)
  (make-array (length string)
              :element-type 'character
              :fill-pointer t
              :initial-contents string))


(defun string->immutable (string)
  (if (subtypep (type-of string) '(simple-array character *))
      string
      (make-array (length string)
                  :element-type 'character
                  :initial-contents string)))


(defun mutate-string (string control-string &rest arguments)
  (setf (fill-pointer string) 0)
  (with-output-to-string (out string)
    (apply #'format out control-string arguments)))
