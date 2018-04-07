(cl:in-package :cl-bodge.physics)


(defclass physics-system (enableable generic-system) ())


(defmethod initialize-system :after ((this physics-system))
  (loop for engine in (list-simulation-engines)
        do (log-errors
             (simulation-engine-initialize engine))))


(defmethod discard-system :after ((this physics-system))
  (loop for engine in (list-simulation-engines)
        do (log-errors
             (simulation-engine-discard engine))))


(definline physics ()
  (engine-system 'physics-system))
