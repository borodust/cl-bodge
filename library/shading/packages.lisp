(in-package :cl-bodge.asdf)

(ge.util:define-package :cl-bodge.library.shading
  (:nicknames :ge.lib.shad)
  (:use :cl :cl-bodge.engine :cl-bodge.graphics :cl-bodge.utils :cl-bodge.resources)
  (:export define-shader-library
           compile-shader-library
           define-shading-program
           build-shading-program
           load-shader-source))


(defpackage :cl-bodge.library.shading.program-descriptor
  (:use))


(defpackage :cl-bodge.library.shading.library-descriptor
  (:use))
