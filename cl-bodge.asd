(in-package :cl-user)

(defpackage :cl-bodge.asdf
  (:use :cl :asdf))

(in-package :cl-bodge.asdf)


(defsystem cl-bodge/engine
  :description "Bodacious Game Engine foundation library"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cl-muth rtg-math log4cl bordeaux-threads local-time
                          trivial-garbage cl-autowrap cl-plus-c)
  :pathname "engine"
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
               (:module memory
                        :serial t
                        :components ((:file "disposable")))
               (:module concurrency
                        :serial t
                        :components ((:file "dispatch")
                                     (:file "transform-dispatch")
                                     (:file "dispatched-defun")
                                     (:file "execution")
                                     (:file "job-queue")))
               (:file "properties")
               (:file "engine")
               (:file "generic-system")
               (:file "thread-bound-system")))


(defsystem cl-bodge/events
  :description "Bodacious Game Engine event system"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine log4cl)
  :pathname "events"
  :serial t
  :components ((:file "packages")
               (:file "system")))


(defsystem cl-bodge/host
  :description "Bodacious Game Engine"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/events cl-glfw3 log4cl bordeaux-threads cl-muth
                               trivial-main-thread)
  :pathname "host"
  :serial t
  :components ((:file "packages")
               (:file "events")
               (:file "system")))


(defsystem cl-bodge/graphics
  :description "Bodacious Game Engine graphics system"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/host cl-opengl log4cl local-time)
  :pathname "graphics"
  :serial t
  :components ((:file "packages")
               (:file "gl")
               (:file "resources")
               (:file "buffers")
               (:file "vertex-array")
               (:file "mesh")
               (:file "shading")
               (:file "textures")
               (:file "system")))


(defsystem cl-bodge/animation
  :description "Bodacious Game Engine animation library"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine)
  :pathname "animation"
  :serial t
  :components ((:file "packages")
               (:file "keyframed")))


(defsystem cl-bodge/audio
  :description "Bodacious Game Engine audio system"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/host log4cl cl-openal cl-alc)
  :pathname "audio"
  :serial t
  :components ((:file "packages")
               (:file "resources")
               (:file "al")
               (:file "buffer")
               (:file "source")
               (:file "system")))


(defsystem cl-bodge/physics
  :description "Bodacious Game Engine physics system"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine bodge-ode log4cl cl-autowrap cl-plus-c local-time)
  :pathname "physics"
  :serial t
  :components ((:file "packages")
               (:file "ode")
               (:file "universe")
               (:file "system")
               (:file "mass")
               (:file "rigid-body")
               (:file "joints")
               (:file "geometry")))


(defsystem cl-bodge/resources
  :description "Bodacious Game Engine resources"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/graphics cl-bodge/audio bodge-sndfile log4cl
                               cl-fad opticl)
  :pathname "resources"
  :serial t
  :components ((:file "packages")
               (:file "resource-loader")
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


(defsystem cl-bodge/scenegraph
  :description "Bodacious Game Engine scenegraph implementation"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/graphics cl-bodge/physics cl-bodge/host cl-muth
                               cl-bodge/animation cl-bodge/resources cl-bodge/audio)
  :pathname "scene"
  :serial t
  :components ((:file "packages")
               (:file "misc")
               (:file "node")
               (:file "scene")
               (:file "transformations")
               (:file "rendering")
               (:file "lighting")
               (:file "animation")
               (:file "model")))


(defsystem cl-bodge/tests
  :description "Test suite for cl-bodge engine"
  :version "0.2.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:module concurrency
                        :serial t
                        :components ((:file "suite")
                                     (:file "dispatch")))))
