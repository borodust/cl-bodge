(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
  (:export engine-resource-name

           encode-resource
           decode-resource

           register-resource
           load-resource
           resource-flow
           list-registered-resource-names

           path-node
           open-resource-stream
           mount-resource-provider
           mount-filesystem

           defresource
           make-resource-handler
           make-text-resource-handler))
