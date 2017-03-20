(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.resources
  (:nicknames :ge.rsc)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :cl-bodge.assets
        :bodge-sndfile)
  (:export make-resource-loader

           mesh-asset-mesh
           mesh-asset-transform
           mesh-asset-bones

           load-ogg-vorbis-audio
           load-png-image))
