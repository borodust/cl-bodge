(in-package :cl-bodge.network)


(declaim (special *socket-regsitry*
                  *write-object-pool*
                  *write-object-registry*))


(defclass network-system (dispatching generic-system)
  ((enabled-p :initform nil :reader enabledp)
   queue executor notifier allocator))


(definline network ()
  (engine-system 'network-system))


(defstruct (write-object
             (:constructor %make-write-object ()))
  (value (ptr (calloc '%uv:write-t)) :type cffi:foreign-pointer :read-only t)
  (buf (ptr (calloc '%uv:buf-t)) :type cffi:foreign-pointer :read-only t))


(defun make-write-object ()
  (let ((obj (%make-write-object)))
    (register-foreign-object *write-object-registry* obj (write-object-value obj))))


(defun dispose-write-object (obj)
  (unregister-foreign-object *write-object-registry* (write-object-value obj))
  (free (write-object-value obj))
  (free (write-object-buf obj)))


(defmethod initialize-system :after ((this network-system))
  (with-slots (executor queue notifier enabled-p allocator) this
    (setf executor (acquire-executor :single-threaded-p t :exclusive-p t)
          queue (make-task-queue))
    (flet ((init ()
             (c-with ((loop-var %uv:loop-t)
                      (notifier-var %uv:async-t))
               (unwind-protect
                    (progn
                      (%uv:loop-init loop-var)
                      (%uv:async-init loop-var notifier-var nil)
                      (setf notifier notifier-var
                            allocator (make-pool-allocator +max-frame-size+)
                            enabled-p t)
                      (let ((*system* this)
                            (*loop-handle* loop-var)
                            (*allocator* allocator)
                            (*socket-regsitry* (make-foreign-object-registry))
                            (*write-object-registry* (make-foreign-object-registry))
                            (*write-object-pool* (make-object-pool #'make-write-object
                                                                   #'dispose-write-object)))
                        (loop while enabled-p
                           do (log-errors
                                (%uv:run loop-var %uv:+run-once+)
                                (drain queue))
                           finally (clear-object-pool *write-object-pool*))))
                 (%uv:loop-close loop-var)
                 (log:trace "async event-loop exited")))))
      (execute executor #'init))))


(defmethod discard-system :before ((this network-system))
  (with-slots (executor queue notifier enabled-p) this
    (mt:wait-with-latch (latch)
      (flet ((stop-loop ()
               (clearup queue)
               (setf enabled-p nil)
               (mt:open-latch latch)))
        (dispatch this #'stop-loop nil)))
    (release-executor executor)))


(defmethod dispatch ((this network-system) (task function) invariant &key &allow-other-keys)
  (with-slots (queue notifier) this
    (push-task task queue)
    (%uv:async-send notifier)))


(definline register-socket (socket)
  (register-foreign-object *socket-regsitry* socket))


(definline pointer->socket (socket-ptr)
  (find-foreign-object *socket-regsitry* socket-ptr))


(definline unregister-socket (socket)
  (unregister-foreign-object *socket-regsitry* socket))
