(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.event
  (:nicknames :ge.eve)
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.concurrency
        :cl :bordeaux-threads :cl-muth)
  (:export event-system
           event
           defevent
           register-event-class
           register-event-classes
           post
           subscribe-to
           subscribe-with-handler-body-to))
