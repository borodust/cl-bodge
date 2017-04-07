(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine)
  (:export register-resource-loader

           +engine-external-resource-prefix+
           engine-resource-name
           engine-external-resource-name

           list-registered-resource-names
           release-resources
           get-resource
           resource-flow

           list-resource-names
           load-resource
           release-resource

           encode-resource
           decode-resource))
