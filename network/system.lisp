(in-package :cl-bodge.network)


(defclass network-system (enableable dispatching generic-system)
  (queue executor notifier))


(definline network ()
  (engine-system 'network-system))


(defmethod initialize-system :after ((this network-system))
  (with-slots (executor queue notifier) this
    (setf executor (acquire-executor :single-threaded-p t :exclusive-p t)
          queue (make-task-queue))
    (labels ((drain-queue ()
               (let ((*system* this))
                 (drain queue)))
             (init ()
               (as:with-event-loop ()
                 (setf notifier (as:make-notifier #'drain-queue :single-shot nil)))
               (log:trace "async event-loop exited")))
      (execute executor #'init))))


(defmethod discard-system :before ((this network-system))
  (with-slots (executor queue notifier) this
    (clearup queue)
    (as:free-notifier notifier)
    (release-executor executor)))


(defmethod dispatch ((this network-system) (task function) invariant &key &allow-other-keys)
  (with-slots (queue notifier) this
    (push-task task queue)
    (as:trigger-notifier notifier)))
