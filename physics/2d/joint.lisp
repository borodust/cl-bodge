(cl:in-package :cl-bodge.physics.chipmunk)


(defhandle constraint-handle
  :closeform (%cp:constraint-free *handle-value*))


(defclass chipmunk-constraint (disposable)
  ((handle :initarg :handle :initform (error ":handle missing") :reader handle-of)
   (universe :initarg :universe :initform (error ":universe missing"))))


(define-destructor chipmunk-constraint (universe (constraint-handle handle-of))
  (%remove-and-free-constraint universe constraint-handle))


(defmethod simulation-engine-destroy-constraint ((this chipmunk-engine)
                                                 (constraint chipmunk-constraint))
  (dispose constraint))


(defmethod simulation-engine-make-damped-spring-constraint ((this chipmunk-engine)
                                                            (universe universe)
                                                            (this-body rigid-body)
                                                            (that-body rigid-body)
                                                            (rest-length number)
                                                            (stiffness number)
                                                            (damping number)
                                                            &key this-anchor that-anchor)
  (with-cp-vects ((anchor-a (or this-anchor *zero-vec2*))
                  (anchor-b (or that-anchor *zero-vec2*)))
    (let ((constraint (make-instance 'chipmunk-constraint
                                     :universe universe
                                     :handle (make-constraint-handle
                                              (%cp:damped-spring-new (handle-value-of this-body)
                                                                     (handle-value-of that-body)
                                                                     anchor-a anchor-b
                                                                     (cp-float rest-length)
                                                                     (cp-float stiffness)
                                                                     (cp-float damping))))))
      (%cp:space-add-constraint (handle-value-of universe) (handle-value-of constraint))
      constraint)))



(defmethod simulation-engine-make-slide-constraint ((this chipmunk-engine)
                                                    (universe universe)
                                                    (this-body rigid-body)
                                                    (that-body rigid-body)
                                                    (min number)
                                                    (max number)
                                                    &key this-anchor that-anchor)
  (with-cp-vects ((anchor-a (or this-anchor *zero-vec2*))
                  (anchor-b (or that-anchor *zero-vec2*)))
    (let ((constraint (make-instance 'chipmunk-constraint
                                     :universe universe
                                     :handle (make-constraint-handle
                                              (%cp:slide-joint-new (handle-value-of this-body)
                                                                   (handle-value-of that-body)
                                                                   anchor-a anchor-b
                                                                   (cp-float min)
                                                                   (cp-float max))))))
      (%cp:space-add-constraint (handle-value-of universe) (handle-value-of constraint))
      constraint)))



(defmethod simulation-engine-make-pin-constraint ((this chipmunk-engine)
                                                  (universe universe)
                                                  (this-body rigid-body)
                                                  that-body
                                                  &key this-anchor that-anchor)
  (with-cp-vects ((anchor-a (or this-anchor *zero-vec2*))
                  (anchor-b (or that-anchor *zero-vec2*)))
    (let ((constraint (make-instance 'chipmunk-constraint
                                     :universe universe
                                     :handle (make-constraint-handle
                                              (%cp:pin-joint-new (handle-value-of this-body)
                                                                 (or (and that-body (handle-value-of that-body))
                                                                     (%cp:space-get-static-body (handle-value-of universe)))
                                                                 anchor-a anchor-b)))))
      (%cp:space-add-constraint (handle-value-of universe) (handle-value-of constraint))
      constraint)))
