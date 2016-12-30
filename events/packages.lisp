(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.event
  (:nicknames :ge.eve)
  (:use :cl :cl-bodge.engine :cl-bodge.utils)
  (:export event-system
           events
           event
           defevent
           register-event-class
           register-event-classes
           post
           subscribe-to
           subscribe-body-to
           define-event-handler))
