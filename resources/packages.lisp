(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
  (:export register-resource-loader

           release-resources
           get-resource
           resource-flow

           engine-resource-id

           list-resource-names
           load-resource
           release-resource

           encode-resource
           decode-resource))
