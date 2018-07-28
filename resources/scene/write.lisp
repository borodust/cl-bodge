(cl:in-package :cl-bodge.resources)


(defun write-sequence-bytes (stream source element-size)
  (let ((byte-length (* (reduce #'* (array-dimensions source)) element-size)))
    (with-simple-array-pointer (source-ptr source)
      (with-static-vectors ((byte-vec byte-length :element-type '(unsigned-byte 8)))
        (claw:memcpy (static-vector-pointer byte-vec) source-ptr
                     :n byte-length :type :unsigned-char)
        (write-sequence byte-vec stream)))))


(defun write-descriptor (stream type &rest params &key &allow-other-keys)
  (with-character-stream (stream)
    (prin1 (nconc (list type) params) stream)))


(defun array-type-info (source)
  (eswitch ((array-element-type source) :test #'subtypep)
    ('single-float (values :float (claw:sizeof :float)))
    ('(unsigned-byte 32) (values :unsigned-int (claw:sizeof :int)))))


(defun write-array (out type array)
  (let ((length (reduce #'* (array-dimensions array))))
    (when (> length 0)
      (multiple-value-bind (element-type element-size)
          (array-type-info array)
        (write-descriptor out type :length length :type element-type)
        (write-sequence-bytes out array element-size)))))


;;;
;;; MESHES
;;;
(defun write-mesh-attributes (out mesh)
  (write-array out :position-array (mesh-position-array mesh))
  (write-array out :index-array (mesh-index-array mesh))
  (write-array out :normal-array (mesh-normal-array mesh)))


(defun write-mesh (out mesh mesh-idx)
  (let ((data (flex:with-output-to-sequence (stream)
                (write-mesh-attributes stream mesh))))
    (write-descriptor out :mesh :index mesh-idx
                                :primitive (mesh-primitive mesh)
                                :size (length data))
    (write-sequence data out)))


(defun write-meshes (out scene)
  (do-meshes (mesh id scene)
    (write-mesh out mesh id)))


(defun write-scene (stream scene)
  (write-meshes stream scene))
