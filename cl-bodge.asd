(in-package :cl-user)

(defpackage :cl-bodge.definition
  (:use :cl :asdf))

(in-package :cl-bodge.definition)

(defsystem cl-bodge
  :description "Bodacious Game Engine"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-opengl cl-glfw3 cl-muth split-sequence sb-cga cffi clode
                          log4cl bordeaux-threads trivial-main-thread cl-openal cl-alc
                          cl-fad local-time)
  :serial t
  :components ((:file "packages")
               (:module utils
                        :serial t
                        :components ((:file "utils")))
               (:module math
                        :serial t
                        :components ((:file "types")
                                     (:file "vector")
                                     (:file "matrix")))
               (:module concurrency
                        :serial t
                        :components ((:file "job-queue")))
               (:module resources
                        :serial t
                        :components ((:file "shader-source")))
               (:module engine
                        :serial t
                        :components ((:file "properties")
                                     (:file "engine")
                                     (:file "thread-bound-system")))
               (:module event
                        :serial t
                        :components ((:file "system")))
               (:module host
                        :serial t
                        :components ((:file "events")
                                     (:file "system")))
               (:module graphics
                        :serial t
                        :components ((:file "gl")
                                     (:file "renderable")
                                     (:file "buffers")
                                     (:file "vertex-array")
                                     (:file "mesh")
                                     (:file "shading")
                                     (:file "system")))
               (:module audio
                        :serial t
                        :components ((:file "al")
                                     (:file "buffer")
                                     (:file "source")
                                     (:file "system")))
               (:module physics
                        :serial t
                        :components ((:file "universe")
                                     (:file "system")
                                     (:file "mass")
                                     (:file "rigid-body")
                                     (:file "joints")
                                     (:file "geometry")))))
