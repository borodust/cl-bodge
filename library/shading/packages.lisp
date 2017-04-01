(in-package :cl-bodge.asdf)

(ge.util:define-package :cl-bodge.library.shading
  (:nicknames :ge.lib.shad)
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.utils :cl-bodge.resources)
  (:export define-shader-library
           define-shading-program
           shading-program-resource-name
           shading-program-descriptor-asset-name
           load-shader-source
           load-shading-program
           build-shading-program))
