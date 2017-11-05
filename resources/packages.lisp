(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
  (:export engine-resource-name

           encode-resource
           decode-resource

           register-resource
           load-resource
           resource-flow
           list-registered-resource-names
           find-resource-handler

           path-node
           open-resource-stream
           mount-resource-provider
           mount-filesystem

           defresource
           make-resource-handler
           make-text-resource-handler))
