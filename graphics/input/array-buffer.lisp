(cl:in-package :cl-bodge.graphics)


(defclass array-buffer-input (disposable shader-input)
  (size length buffer-id type))


(define-destructor array-buffer-input (buffer-id)
  (dispose-gl-object (list buffer-id) #'gl:delete-buffers))


(defun update-array-buffer (array-buffer data &optional size)
  (with-slots (buffer-id type (this-size size)) array-buffer
    (gl:bind-buffer :array-buffer buffer-id)
    (let ((*buffer-target* :array-buffer))
      (multiple-value-bind (element-type computed-size)
          (fill-buffer (apply #'aref data (mapcar (constantly 0) (array-dimensions data))) data)
        (setf type element-type
              this-size (or computed-size size))))
    (gl:bind-buffer :array-buffer 0)))


(defmethod initialize-instance :after ((this array-buffer-input) &key data size)
  (with-slots (buffer-id type (this-size size)) this
    (when (zerop (array-dimension data 0))
      (error "Arrays of zero length are not tolerated"))
    (let ((id (gl:gen-buffer)))
      (bind-for-serious-condition ((lambda () (gl:delete-buffers (list id))))
        (gl:bind-buffer :array-buffer id)
        (setf buffer-id id)
        (update-array-buffer this data size)))))


(defun make-array-buffer (array &key element-size)
  (make-instance 'array-buffer-input :data array :size element-size))


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
