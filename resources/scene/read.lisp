(cl:in-package :cl-bodge.resources)

(declaim (special *scene-read-buffer*))

(defparameter *scene-read-buffer-size* (* 64 1024))


(defmacro with-character-stream ((stream &key (external-format :utf-8)) &body body)
  `(let ((,stream (flexi-streams:make-flexi-stream ,stream :external-format ,external-format)))
     ,@body))


(defun read-descriptor (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (block nil
        (handler-bind ((end-of-file (lambda (c) (declare (ignore c)) (return))))
          (read-preserving-whitespace stream nil nil nil))))))


(defun for-each-descriptor (stream action)
  (with-character-stream (stream)
    (loop for descriptor = (read-descriptor stream)
          while descriptor
          do (destructuring-bind (descriptor-type &rest args &key &allow-other-keys)
                 descriptor
               (apply action descriptor-type args)))))


(defmacro do-descriptors ((descriptor-lambda-list stream) &body body)
  (with-gensyms (args)
    `(for-each-descriptor ,stream (lambda (&rest ,args)
                                    (destructuring-bind (,@descriptor-lambda-list) ,args
                                      ,@body)))))


(defun read-array (stream &key (length (error ":count missing"))
                            ((:type element-type) (error ":element-type missing"))
                   &allow-other-keys)
  (let* ((type (ecase element-type
                 (:float 'single-float)
                 (:unsigned-int '(unsigned-byte 32))))
         (byte-length (* length (claw:sizeof element-type)))
         (array (make-array length :element-type type)))
    (with-simple-array-pointer (dst-ptr array)
      (loop with byte-offset = 0
            with dst-addr = (cffi:pointer-address dst-ptr)
            with src-ptr = (static-vectors:static-vector-pointer *scene-read-buffer*)
            for bytes-read = (read-sequence *scene-read-buffer* stream
                                            :end (min (- byte-length byte-offset)
                                                      *scene-read-buffer-size*))
            while (> bytes-read 0)
            do (claw:memcpy (cffi:make-pointer (+ dst-addr byte-offset))
                            src-ptr
                            :n bytes-read
                            :type :char)
               (incf byte-offset bytes-read)
            finally (unless (= byte-offset byte-length)
                      (error "Premature end of stream: expected ~A, got ~A"
                             byte-length byte-offset))))
    array))


(defun read-mesh (stream &key index primitive size)
  (let ((stream (make-bounded-input-stream stream size))
        (mesh (make-mesh-resource primitive)))
    (do-descriptors ((attribute-type &rest args) stream)
      (ecase attribute-type
        (:position-array (setf (mesh-resource-position-array mesh) (apply #'read-array stream args)))
        (:index-array (setf (mesh-resource-index-array mesh) (apply #'read-array stream args)))
        (:normal-array (setf (mesh-resource-normal-array mesh) (apply #'read-array stream args)))))
    (values mesh index)))


(defun read-material (stream &key index
                               name
                               shininess
                               diffuse-color
                               emissive-color
                               base-color-factor
                               metallic-factor
                               roughness-factor
                               glossiness-factor
                               alpha-mode
                               alpha-cutoff
                               textures &allow-other-keys)
  (declare (ignore stream))
  (let ((material (make-material-resource :name name
                                          :shininess shininess
                                          :diffuse-color diffuse-color
                                          :emissive-color emissive-color
                                          :base-color-factor base-color-factor
                                          :metallic-factor metallic-factor
                                          :roughness-factor roughness-factor
                                          :glossiness-factor glossiness-factor
                                          :alpha-mode alpha-mode
                                          :alpha-cutoff alpha-cutoff)))
    (loop for texture in textures
          do (destructuring-bind (&key type
                                    id
                                    name
                                    channel
                                    coord-id
                                    mapping-id
                                    mapping-name
                                    mapping-mode-u
                                    mapping-mode-v
                                    mapping-filter-mag
                                    mapping-filter-min
                                    scale
                                    strength
                                  &allow-other-keys)
                 texture
               (setf (material-resource-texture material type id)
                     (make-texture-resource
                      :name               name
                      :channel            channel
                      :coord-id           coord-id
                      :mapping-id         mapping-id
                      :mapping-name       mapping-name
                      :mapping-mode-u     mapping-mode-u
                      :mapping-mode-v     mapping-mode-v
                      :mapping-filter-mag mapping-filter-mag
                      :mapping-filter-min mapping-filter-min
                      :scale              scale
                      :strength           strength))))
    (values material index)))


(defun read-scene (stream)
  (static-vectors:with-static-vector (*scene-read-buffer* *scene-read-buffer-size*)
    (let ((scene (make-empty-scene-resource)))
      (do-descriptors ((type &rest params) stream)
        (ecase type
          (:mesh (multiple-value-bind (mesh id)
                     (apply #'read-mesh stream params)
                   (setf (scene-resource-mesh scene id) mesh)))
          (:material (multiple-value-bind (material id)
                         (apply #'read-material stream params)
                       (setf (scene-resource-material scene id) material)))))
      scene)))
