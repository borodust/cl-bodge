(in-package :cl-bodge.asdf)


(defpackage :cl-bodge.assets
  (:nicknames :ge.as)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :bodge-sndfile :cl-bodge.resources)
  (:export make-resource-loader

           mesh-asset-mesh
           mesh-asset-transform
           mesh-asset-bones

           load-ogg-vorbis-audio
           load-png-image

           font-atlas-asset-id
           font-asset-id))
