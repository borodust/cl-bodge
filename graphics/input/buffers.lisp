(cl:in-package :cl-bodge.graphics)


(defgeneric fill-buffer (test-value data &optional type))






(defun extract-element-info (array)
  (let* ((dimensions (array-dimensions array))
         (element-count (or (second dimensions) 1)))
    (when (> element-count 4)
      (error "Unsupported simple array: too many elements"))
    (when (third dimensions)
      (error "Unsupported simple array: too many dimensions"))
    (let ((gl-type (cl-type->gl (array-element-type array))))
      (values element-count dimensions gl-type (when gl-type (gl-type-size gl-type))))))


(defun fill-with-simple-array (data)
  (multiple-value-bind (element-count dimensions gl-type element-byte-size)
      (extract-element-info data)
    (let ((byte-length (* (first dimensions) element-byte-size element-count)))
      (with-simple-array-pointer (ptr data)
        (%gl:buffer-data *buffer-target* byte-length (cffi:null-pointer) :static-draw)
        (%gl:buffer-sub-data *buffer-target* 0 byte-length ptr))
      (values gl-type element-count))))


(defun fill-with-number-array (data target-type converter)
  (multiple-value-bind (element-count dimensions) (extract-element-info data)
    (let* ((target-cl-type (gl-type->cl target-type))
           (element-length (* (first dimensions) element-count))
           (byte-length (* element-length (gl-type-size target-type))))
      (static-vectors:with-static-vector (svec element-length :element-type target-cl-type)
        (if (second dimensions)
            (loop for i from 0 below (first dimensions)
                  do (loop for j from 0 below element-count
                           for value = (funcall converter (aref data i j))
                           do (setf (aref svec (+ (* i element-count) j)) value)))
            (loop for i from 0
                  for value across data
                  do (setf (aref svec i) (funcall converter value))))
        (%gl:buffer-data *buffer-target* byte-length (cffi:null-pointer) :static-draw)
        (%gl:buffer-sub-data *buffer-target* 0 byte-length
                             (static-vectors:static-vector-pointer svec)))
      (values target-type element-count))))


(defun fill-number-buffer (data type)
  (let ((element-type (cl-type->gl (array-element-type data))))
    (if (and (eq type element-type) (acceptable-simple-array-p data))
        (fill-with-simple-array data)
        (let ((cl-type (gl-type->cl type)))
          (flet ((%coerce-to-int (v)
                   (coerce (floor v) cl-type))
                 (%coerce (v)
                   (coerce v cl-type)))
            (fill-with-number-array data type (if (subtypep cl-type 'integer)
                                                  #'%coerce-to-int
                                                  #'%coerce)))))))


(defmethod fill-buffer ((value number) data &optional (type :float))
  (fill-number-buffer data type))


(defun fill-with-vec-array (element-size data)
  (let ((byte-size 4))
    (static-vectors:with-static-vector (svec (* (length data) element-size)
                                             :element-type 'single-float)
      (loop for i from 0
            for value across data
            for svec-start = (* i element-size)
            for svec-end = (+ svec-start element-size)
            do (loop for j from svec-start below svec-end
                     for k from 0
                     do (setf (aref svec j) (vref value k))))
      (let ((byte-length (* (length svec) byte-size)))
        (%gl:buffer-data *buffer-target* byte-length (cffi:null-pointer) :static-draw)
        (%gl:buffer-sub-data *buffer-target* 0 byte-length
                             (static-vectors:static-vector-pointer svec)))
      (values :float element-size))))


(defmethod fill-buffer ((value vec2) data &optional (type :float))
  (unless (eq type :float)
    (warn "Unexpected uniform vector type"))
  (fill-with-vec-array 2 data))


(defmethod fill-buffer ((value vec3) data &optional (type :float))
  (unless (eq type :float)
    (warn "Unexpected uniform vector type"))
  (fill-with-vec-array 3 data))


(defmethod fill-buffer ((value vec4) data &optional (type :float))
  (unless (eq type :float)
    (warn "Unexpected uniform vector type"))
  (fill-with-vec-array 4 data))
