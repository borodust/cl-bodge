(cl:in-package :cl-bodge.graphics)

(declaim (special *active-shading-program*
                  *buffer-target*))

(defclass shader-input () ())


(defun select-primitive-type (value)
  (typecase value
    (single-float (values :float 4))
    (double-float (values :double 8))
    ((signed-byte 8) (values :byte 1))
    ((unsigned-byte 8) (values :unsigned-byte 1))
    ((signed-byte 16) (values :short 2))
    ((unsigned-byte 16) (values :unsigned-short 2))
    ((signed-byte 32) (values :int 4))
    ((unsigned-byte 32) (values :unsigned-int 4))))


(defgeneric inject-shader-input (shader-input &rest args &key &allow-other-keys))


(defclass array-buffer-input (disposable shader-input)
  (size length buffer-id type))


(define-destructor array-buffer-input (buffer-id)
  (gl:delete-buffers (list buffer-id)))


(defgeneric fill-buffer (type data))


(definline acceptable-simple-array-p (data)
  (and (typep data 'simple-array) (not (typep data 'simple-vector))))


(defun fill-with-simple-array (element-byte-size data)
  (let ((byte-length (* (length data) element-byte-size)))
    (with-simple-array-pointer (ptr data)
      (%gl:buffer-data *buffer-target* byte-length (cffi:null-pointer) :static-draw)
      (%gl:buffer-sub-data *buffer-target* 0 byte-length ptr))))


(defun fill-with-number-array (data type element-byte-size converter)
  (let ((byte-length (* (length data) element-byte-size)))
    (static-vectors:with-static-vector (svec (length data) :element-type type)
      (loop for i from 0
            for value across data
            do (setf (aref svec i) (funcall converter (aref data i))))
      (%gl:buffer-data *buffer-target* byte-length (cffi:null-pointer) :static-draw)
      (%gl:buffer-sub-data *buffer-target* 0 byte-length
                           (static-vectors:static-vector-pointer svec)))))


(defun fill-with-floats (data)
  (if (and (> (length data) 0)
           (typep (aref data 0) 'single-float)
           (acceptable-simple-array-p data))
      (fill-with-simple-array 4 data)
      (flet ((to-float (number)
               (f number)))
        (fill-with-number-array data 'single-float 4 #'to-float)))
  :float)


(defun fill-with-integers (data)
  (if (and (> (length data) 0)
           (typep (aref data 0) '(unsigned-byte 32))
           (acceptable-simple-array-p data))
      (fill-with-simple-array 4 data)
      (flet ((to-integer (number)
               (coerce number '(unsigned-byte 32))))
        (fill-with-number-array data '(unsigned-byte 32) 4 #'to-integer)))
  :unsigned-int)


(defmethod fill-buffer ((value number) data)
  (multiple-value-bind (type byte-size) (select-primitive-type value)
    (if (and type (acceptable-simple-array-p data))
        (prog1 type
          (fill-with-simple-array byte-size data))
        (fill-with-floats data))))


(defun fill-with-vec-array (element-size data)
  (let ((byte-size 4))
    (static-vectors:with-static-vector (svec (* (length data) element-size) :element-type 'single-float)
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


(defmethod fill-buffer ((value vec2) data)
  (fill-with-vec-array 2 data))


(defmethod fill-buffer ((value vec3) data)
  (fill-with-vec-array 3 data))


(defmethod fill-buffer ((value vec4) data)
  (fill-with-vec-array 4 data))


(defmethod initialize-instance :after ((this array-buffer-input) &key data size)
  (with-slots (buffer-id length type (this-size size)) this
    (when (zerop (length data))
      (error "Arrays of zero length are not tolerated"))
    (let ((id (gl:gen-buffer)))
      (bind-for-serious-condition ((lambda () (gl:delete-buffers (list id))))
        (gl:bind-buffer :array-buffer id)
        (let ((*buffer-target* :array-buffer))
          (multiple-value-bind (element-type computed-size) (fill-buffer (aref data 0) data)
            (setf buffer-id id
                  length (length data)
                  type element-type
                  this-size (or computed-size size))))
        (gl:bind-buffer :array-buffer 0)))))


(defun* make-array-buffer ((data (simple-array * *)) &key element-size)
  (make-instance 'array-buffer-input :data data :size element-size))


(defmethod inject-shader-input ((this array-buffer-input) &key
                                                            location
                                                            name
                                                            ((:type expected-type))
                                                            ((:size expected-size)))
  (with-slots (size buffer-id type) this
    (when (and expected-type (not (eq expected-type type)))
      (error "Unexpected array buffer type: wanted ~A, got ~A" expected-type type))
    (when (and size expected-size (/= expected-size size))
      (error "Unexpected element size: wanted ~A, got ~A" expected-size size))
    (let ((size (or size expected-size (error "Element size unspecified")))
          (location (or location
                        (when name
                          (gl:get-attrib-location *active-shading-program* name))
                        (error ":name or :location missing"))))
      (when (= location -1)
        (error "Attribute with name ~A not found or unused" name))
      (gl:enable-vertex-attrib-array location)
      (gl:bind-buffer :array-buffer buffer-id)
      (case type
        (:float
         (gl:vertex-attrib-pointer location size type nil 0 (cffi:null-pointer)))
        (:double
         (%gl:vertex-attrib-lpointer location size type 0 (cffi:null-pointer)))
        (t
         (%gl:vertex-attrib-ipointer location size type 0 (cffi:null-pointer))))
      (gl:bind-buffer :array-buffer 0))))


(defmethod inject-shader-input ((value integer) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (gl:uniformi location value)))


(defmethod inject-shader-input ((value single-float) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (%gl:uniform-1f location value)))


(defmethod inject-shader-input ((value double-float) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (%gl:uniform-1d location value)))


(defmethod inject-shader-input ((value vec) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (gl:uniformfv location (vec->array value))))


(defmethod inject-shader-input ((value mat4) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-4fv location 1 nil ptr))))


(defmethod inject-shader-input ((value mat3) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-3fv location 1 nil ptr))))


(defmethod inject-shader-input ((value mat2) &key name)
  (let ((location (gl:get-uniform-location *active-shading-program* name)))
    (with-simple-array-pointer (ptr (mat->array value))
      (%gl:uniform-matrix-2fv location 1 nil ptr))))


(defgeneric index-array-length (array)
  (:method ((array null)) 0))


(defclass index-array-input (disposable shader-input)
  ((buffer-id)
   (length :reader index-array-length)))


(define-destructor index-array-input (buffer-id)
  (gl:delete-buffers (list buffer-id)))


(defmethod initialize-instance :after ((this index-array-input) &key data)
  (with-slots (buffer-id length) this
    (let ((id (gl:gen-buffer)))
      (bind-for-serious-condition ((lambda () (gl:delete-buffers (list id))))
        (gl:bind-buffer :element-array-buffer id)
        (let ((*buffer-target* :element-array-buffer))
          (fill-with-integers data))
        (setf buffer-id id
              length (length data))
        (gl:bind-buffer :element-array-buffer 0)))))


(defun make-index-array (data)
  (make-instance 'index-array-input :data data))


(defmethod inject-shader-input ((this index-array-input) &key)
  (with-slots (buffer-id) this
    (gl:bind-buffer :element-array-buffer buffer-id)))


(defun make-texture (data))
