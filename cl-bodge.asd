(in-package :cl-user)

(defpackage :cl-bodge.definition
  (:use :cl :asdf))

(in-package :cl-bodge.definition)

(defsystem cl-bodge
  :description "Bodacious Game Engine"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-opengl cl-glfw3 cl-muth rtg-math cffi clode bodge-sndfile
                          log4cl bordeaux-threads trivial-main-thread cl-openal cl-alc
                          cl-fad local-time blackbird trivial-garbage opticl)
  :serial t
  :components ((:file "packages")
               (:module utils
                        :serial t
                        :components ((:file "utils")))
               (:module math
                        :serial t
                        :components ((:file "types")
                                     (:file "scalar")
                                     (:file "vector")
                                     (:file "matrix")
                                     (:file "matvec")
                                     (:file "quaternion")))
               (:module concurrency
                        :serial t
                        :components ((:file "async")
                                     (:file "execution")
                                     (:file "job-queue")))
               (:module memory
                        :serial t
                        :components ((:file "disposable")))
               (:module engine
                        :serial t
                        :components ((:file "properties")
                                     (:file "engine")
                                     (:file "generic-system")
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
                                     (:file "resources")
                                     (:file "buffers")
                                     (:file "vertex-array")
                                     (:file "mesh")
                                     (:file "shading")
                                     (:file "textures")
                                     (:file "system")))
               (:module animation
                        :serial t
                        :components ((:file "skeleton")))
               (:module audio
                        :serial t
                        :components ((:file "resources")
                                     (:file "al")
                                     (:file "buffer")
                                     (:file "source")
                                     (:file "system")))
               (:module physics
                        :serial t
                        :components ((:file "universe")
                                     (:file "system")
                                     (:file "ode")
                                     (:file "mass")
                                     (:file "rigid-body")
                                     (:file "joints")
                                     (:file "geometry")))
               (:module resources
                        :serial t
                        :components ((:file "resource-loader")
                                     (:file "basic-chunks")
                                     (:file "simple-model-chunk")
                                     (:file "shader-source")
                                     (:file "shader-library")
                                     (:file "image")
                                     (:file "audio")
                                     (:module shaders
                                              :components
                                              ((:file "math")
                                               (:file "lighting")
                                               (:file "skeleton")))
                                     (:file "system")))
               (:module scene
                        :serial t
                        :components ((:file "utils")
                                     (:file "node")
                                     (:file "scene")
                                     (:file "transformations")
                                     (:file "rendering")
                                     (:file "lighting")
                                     (:file "animation")
                                     (:file "model")))))
