(in-package :cl-bodge.network)


(defstruct slab
  (next nil :type (or null slab))
  (prev nil :type (or null slab))
  (buffer nil :type (or (simple-array (unsigned-byte 8) (*)) null) :read-only t))


(definline slab-ptr (slab)
  (static-vectors:static-vector-pointer (slab-buffer slab)))


(defclass pool-allocator (lockable)
  ((slab-size :initarg :slab-size :reader slab-size-of)
   (slab-table :initform (tg:make-weak-hash-table :weakness :value))
   (used-pool :initform nil)
   (available-pool :initform nil)))


(defun %ensure-slabs-available (allocator)
  (with-slots (available-pool slab-size slab-table) allocator
    (unless available-pool
      (setf available-pool (make-slab :buffer (static-vectors:make-static-vector
                                               slab-size :element-type '(unsigned-byte 8)))
            (gethash (cffi:pointer-address (slab-ptr available-pool)) slab-table) available-pool))))


(defun %pick-slab (allocator)
  (with-slots (available-pool used-pool) allocator
    (let ((result available-pool))
      (when used-pool
        (setf (slab-prev used-pool) result))
      (setf available-pool (slab-next result)
            (slab-next result) used-pool
            used-pool result)
      result)))


(defun %return-slab (allocator slab)
  (with-slots (available-pool used-pool) allocator
    (if (slab-prev slab)
        (setf (slab-next (slab-prev slab)) (slab-next slab))
        (setf used-pool (slab-next slab)))
    (setf (slab-prev slab) nil
          (slab-next slab) available-pool
          available-pool slab)))


(definline make-pool-allocator (slab-size)
  (make-instance 'pool-allocator :slab-size slab-size))


(defun allocate-slab (allocator)
  (with-slots (slab-table) allocator
    (with-instance-lock-held (allocator)
      (%ensure-slabs-available allocator)
      (%pick-slab allocator))))


(defun free-slab (allocator slab)
  (with-instance-lock-held (allocator)
    (%return-slab allocator slab)))


(defun free-slab-by-pointer (allocator pointer)
  (with-slots (slab-table) allocator
    (with-instance-lock-held (allocator)
      (%return-slab allocator (gethash (cffi:pointer-address pointer) slab-table)))))


(defun find-slab (allocator pointer)
  (with-slots (slab-table) allocator
    (with-instance-lock-held (allocator)
      (gethash (cffi:pointer-address pointer) slab-table))))
