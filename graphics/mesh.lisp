(in-package :cl-bodge.graphics)


(defenum face-type
  :points :lines :line-strip :triangles :triangle-strip
  :lines-adjacency :triangles-adjacency)


(defgeneric render-mesh (mesh start end))


(defclass mesh (disposable)
  ((vertex-array :initarg :vertex-array :reader vertex-array-of)
   (primitive-type :initarg :primitive-type)))


(define-destructor mesh (vertex-array)
  (dispose vertex-array))


(defmethod render-mesh ((this mesh) start end)
  (with-slots (vertex-array primitive-type) this
    (with-bound-vertex-array (vertex-array)
      (gl:draw-arrays primitive-type start (- end start)))))


(defmethod render ((this mesh))
  (with-slots (vertex-array primitive-type) this
    (render-mesh this 0 (vertex-count-of vertex-array))))

;;;
;;;
;;;
(defclass indexed-mesh (mesh)
  ((index-buffer :initarg :index-buffer)))


(define-destructor indexed-mesh (index-buffer)
  (dispose index-buffer))


(defun make-mesh (vertex-count primitive-type &optional index-array)
  (declare (type face-type primitive-type))
  (if (null index-array)
      (make-instance 'mesh
                     :vertex-array (make-vertex-array vertex-count)
                     :primitive-type primitive-type)
      (make-instance 'indexed-mesh
                     :vertex-array (make-vertex-array vertex-count)
                     :primitive-type primitive-type
                     :index-buffer (make-index-buffer index-array))))


(defmethod render-mesh ((this indexed-mesh) start end)
  (with-slots (vertex-array primitive-type index-buffer) this
    (with-bound-vertex-array (vertex-array)
      (with-bound-buffer (index-buffer)
	(claw:with-float-traps-masked ()
	  (%gl:draw-range-elements primitive-type start (1- end) (- end start)
				   :unsigned-int (cffi:null-pointer)))))))


(defmethod render ((this indexed-mesh))
  (with-slots (vertex-array primitive-type index-buffer) this
    (render-mesh this 0 (index-count-of index-buffer))))

;;;
;;;
;;;
(defclass patch ()
  ((patch-size :initarg :patch-size :type positive-integer)
   (outer-level :initarg :outer-level :type (or null vec4))
   (inner-level :initarg :inner-level :type (or null vec2))))


(defclass patch-mesh (patch mesh) ())
(defclass indexed-patch-mesh (patch inexed-mesh) ())


(defun make-patch-mesh (vertex-count
                        &key index-array (patch-size 1)
                          (inner-tessellation-level (vec2 1.0 1.0))
                          (outer-tesselation-level (vec4 1.0 1.0 1.0 1.0)))
  (if (null index-array)
      (make-instance 'patch-mesh
                     :vertex-array (make-vertex-array vertex-count)
                     :primitive-type :patches
                     :patch-size patch-size
                     :inner-level inner-tessellation-level
                     :outer-level outer-tesselation-level)
      (make-instance 'indexed-patch-mesh
                     :vertex-array (make-vertex-array vertex-count)
                     :index-buffer (make-index-buffer index-array)
                     :primitive-type :patches
                     :patch-size patch-size
                     :inner-level inner-tessellation-level
                     :outer-level outer-tesselation-level)))


(defmethod render ((this patch))
  (with-slots (patch-size outer-level inner-level) this
    (gl:patch-parameter :patch-vertices patch-size)
    (unless (null outer-level)
      (gl:patch-parameter :patch-default-outer-level (vec->array outer-level)))
    (unless (null inner-level)
      (gl:patch-parameter :patch-default-inner-level (vec->array inner-level)))
    (call-next-method)))


(defmethod attach-array-buffer (buffer (mesh mesh) index)
  (attach-array-buffer buffer (vertex-array-of mesh) index))

;;;
;;;
;;;
(defclass submesh ()
  ((mesh :initarg :mesh)
   (start :initarg :start)
   (end :initarg :end)))


(defun submesh (mesh start-element end-element)
  (make-instance 'submesh :mesh mesh :start start-element :end end-element))


(defmethod render ((this submesh))
  (with-slots (mesh start end) this
    (render-mesh mesh start end)))

;;;
;;; Build from resource
;;;

(define-system-function build-mesh graphics-system (mesh-chunk)
  (let* ((arrays (ge.rsc:mesh-chunk-arrays mesh-chunk))
         (v-count (length (cadar arrays)))
         (chunk-bones (ge.rsc:mesh-chunk-bones mesh-chunk))
         (bone-count (reduce #'max chunk-bones :key #'ge.rsc:mesh-bone-index :initial-value 0))
         (bones
          (loop with r = (make-array bone-count :initial-element nil)
             for bone in chunk-bones do
               (setf (aref r (1- (ge.rsc:mesh-bone-index bone)))
                     (when-let ((skeleton-bone (ge.rsc:mesh-bone-bone bone)))
                       (cons (ge.rsc:id-of skeleton-bone)
                             (sequence->mat4 (ge.rsc:mesh-bone-offset bone)))))
             finally (return r)))
         (index-array (list->array (ge.rsc:mesh-chunk-indexes mesh-chunk)))
         (mesh (ge.gx:make-mesh v-count :triangles index-array)))
    (loop for (array-id array) in arrays do
         (with-disposable ((vbuf (ge.gx:make-array-buffer
                                  (list->array array v-count (length (car array))))))
           (ge.gx:attach-array-buffer vbuf mesh array-id)))
    (values mesh (sequence->mat4 (ge.rsc:mesh-chunk-transform mesh-chunk)) bones)))
