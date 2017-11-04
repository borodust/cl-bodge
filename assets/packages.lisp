(in-package :cl-bodge.asdf)


(ge.util:define-package :cl-bodge.assets
  (:nicknames :ge.as)
  (:use :cl :cl-bodge.utils :cl-bodge.engine :bodge-sndfile :cl-bodge.resources)
  (:export mount-container

           define-sdf-font
           build-sdf-font

           mesh-asset-mesh
           mesh-asset-transform
           mesh-asset-bones

           load-ogg-vorbis-audio
           load-png-image

           font-atlas-resource-name
           font-resource-name))
