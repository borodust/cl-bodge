(in-package :cl-bodge.definition)


(defpackage :cl-bodge.utils
  (:nicknames :bge.util)
  (:use :cl :alexandria)
  (:export log-errors
           read-file-into-string-list))


(defpackage :cl-bodge.concurrency
  (:nicknames :bge.muth)
  (:use :cl :alexandria :bordeaux-threads :cl-muth)
  (:export make-job-queue
           push-job
           push-body-into
           drain))


(defpackage :cl-bodge.math
  (:nicknames :bge.math)
  (:use :cl :alexandria)
  (:export vec
           vec4
           vector->array

           matrix
           mat4
           matrix->array
           identity-matrix
           rotation-matrix
           translation-matrix
           scaling-matrix
           perspective-projection-matrix
           m*))


(defpackage :cl-bodge.engine
  (:nicknames :bge.ng)
  (:use  :cl-bodge.utils
         :cl :alexandria)
  (:export system
           enable
           disable

           engine-system
           startup
           shutdown))


(defpackage :cl-bodge.application
  (:nicknames :bge.app)
  (:use :cl-bodge.engine :cl-bodge.utils :cl-bodge.concurrency
       :cl :bordeaux-threads :alexandria :cl-muth :trivial-main-thread)
  (:export application-system
           
           bind-rendering-context
           swap-buffers))


(defpackage :cl-bodge.graphics
  (:nicknames :bge.gx)
  (:use  :cl-bodge.engine :cl-bodge.application :cl-bodge.concurrency :cl-bodge.utils
         :cl-bodge.math
        :cl :alexandria :cl-muth :bordeaux-threads)
  (:export graphics-system
           render-scene
           within-rendering-context

           render
           rendering-group
           add-renderable

           make-vertex-array

           make-array-buffer
           attach-buffer

           make-shading-program
           use-shading-program
           program-uniform-variable))


(defpackage :cl-bodge
  (:use :cl)
  (:nicknames :bge)
  (:export))
