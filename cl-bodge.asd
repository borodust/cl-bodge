(cl:defpackage :cl-bodge.asdf
  (:use :cl :asdf))
(cl:in-package :cl-bodge.asdf)

(defsystem cl-bodge/utils
  :description "Bodacious Game Engine random utilities"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria uiop log4cl local-time dissect split-sequence cl-autowrap
                          static-vectors)
  :pathname "utils/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "arrays")))


(defsystem cl-bodge/engine
  :description "Bodacious Game Engine foundation library"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-muth rtg-math log4cl bordeaux-threads local-time
                              trivial-garbage uiop cffi cl-flow uiop)
  :pathname "engine/"
  :serial t
  :components ((:file "packages")
               (:module math
                        :serial t
                        :components ((:file "types")
                                     (:file "scalar")
                                     (:file "vector")
                                     (:file "matrix")
                                     (:file "quaternion")))
               (:module memory
                        :serial t
                        :components ((:file "disposable")
                                     (:file "foreign-array")))
               (:module concurrency
                        :serial t
                        :components ((:file "dispatch")
                                     (:file "execution")
                                     (:file "task-queue")
                                     (:file "instance-lock")))
               (:module resources
                        :components ((:file "audio")
                                     (:file "graphics")))
               (:module events
                        :components ((:file "event")
                                     (:file "emitter")
                                     (:file "listener")
                                     (:file "hub")))
               (:file "properties")
               (:file "engine")
               (:file "handle")
               (:file "event")
               (:file "generic-system")
               (:file "thread-bound-system")))


(defsystem cl-bodge/host
  :description "Bodacious Game Engine host system"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-glfw3 log4cl bordeaux-threads
                               cl-muth trivial-main-thread)
  :pathname "host/"
  :serial t
  :components ((:file "packages")
               (:file "events")
               (:file "system")
               (:file "keymap")))


(defsystem cl-bodge/network
  :description "Bodacious Game Engine networking utilities"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-async cl-conspack log4cl closer-mop
                               flexi-streams trivial-gray-streams cl-async)
  :pathname "network/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "message")
               (:file "protocol")
               (:file "conduit")
               (:file "system")
               (:file "server")
               (:file "client")))


(defsystem cl-bodge/graphics
  :description "Bodacious Game Engine graphics system"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host cl-opengl
                               bodge-glad log4cl local-time cffi)
  :pathname "graphics/"
  :serial t
  :components ((:file "packages")
               (:file "gl")
               (:file "buffers")
               (:file "vertex-array")
               (:file "mesh")
               (:file "shading")
               (:file "textures")
               (:file "framebuffer")
               (:file "state")
               (:file "system")))


(defsystem cl-bodge/canvas
  :description "Bodacious Game Engine vector graphics system"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics
                               log4cl bodge-nanovg)
  :pathname "canvas/"
  :serial t
  :components ((:file "packages")
               (:file "canvas")
               (:file "image")
               (:file "paint")
               (:file "primitives")))


(defsystem cl-bodge/animation
  :description "Bodacious Game Engine animation library"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics)
  :pathname "animation/"
  :serial t
  :components ((:file "packages")
               (:file "keyframed")
               (:file "stream")))


(defsystem cl-bodge/audio
  :description "Bodacious Game Engine audio system"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host log4cl
                               cl-openal cl-alc)
  :pathname "audio/"
  :serial t
  :components ((:file "packages")
               (:file "al")
               (:file "buffer")
               (:file "source")
               (:file "system")))


(defsystem cl-bodge/physics
  :description "Bodacious Game Engine physics system"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine bodge-ode log4cl cl-autowrap cl-plus-c local-time)
  :pathname "physics/"
  :serial t
  :components ((:file "packages")
               (:file "ode")
               (:file "contacts")
               (:file "universe")
               (:file "system")
               (:file "mass")
               (:file "rigid-body")
               (:file "joints")
               (:file "geometry")))


(defsystem cl-bodge/resources
  :description "Bodacious Game Engine resource management"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils)
  :pathname "resources/"
  :serial t
  :components ((:file "packages")
               (:file "serialization")
               (:file "registry")))


(defsystem cl-bodge/shading-library
  :description "Bodacious Game Engine shading library"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/resources cl-bodge/graphics flexi-streams)
  :pathname "library/shading/"
  :serial t
  :components ((:file "packages")
               (:file "lighting")
               (:file "shader-source")
               (:file "shader-library")
               (:file "shading-program")
               (:module shaders
                        :components
                        ((:file "math")
                         (:file "lighting")
                         (:file "banner")
                         (:file "skinning")))))


(defsystem cl-bodge/text
  :description "Bodacious Game Engine text rendering"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics cl-bodge/shading-library log4cl)
  :pathname "text/"
  :serial t
  :components ((:file "packages")
               (:file "font")
               (:file "text")
               (:file "rated-queue")
               (:file "text-cache")
               (:module shaders
                        :components ((:file "text-program/text")
                                     (:file "text-library/text")))
               (:file "text-renderer")))


(defsystem cl-bodge/poiu
  :description "Bodacious Game Engine Plain Old Interface for Users"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics bodge-nuklear
                               cl-bodge/text cl-bodge/canvas cl-autowrap cl-plus-c)
  :pathname "poiu/"
  :serial t
  :components ((:file "packages")
               (:file "poiu")
               (:file "events")
               (:file "elements")
               (:file "rendering-backend")))


(defsystem cl-bodge/scenegraph
  :description "Bodacious Game Engine scenegraph implementation"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/graphics cl-bodge/physics
                               cl-bodge/host cl-muth cl-bodge/animation
                               cl-bodge/audio cl-bodge/shading-library)
  :pathname "scene/"
  :serial t
  :components ((:file "packages")
               (:file "node")
               (:file "scene")
               (:file "simulation")
               (:module rendering
                        :serial t
                        :components ((:file "rendering-pass")
                                     (:file "shading")
                                     (:file "texturing")
                                     (:file "lighting")
                                     (:file "mesh")
                                     (:file "banner")))
               (:file "transformations")
               (:file "animation")
               (:file "model")))


(defsystem cl-bodge/interactions
  :description "Interactive components of Bodacious Game Engine"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/poiu cl-bodge/physics
                               cl-bodge/host cl-bodge/scenegraph)
  :pathname "interactions/"
  :serial t
  :components ((:file "packages")
               (:file "input-state")
               (:file "control-ray")
               (:file "board")
               (:file "system")))


(defsystem cl-bodge/assets
  :description "Bodacious Game Engine .BRF"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils  cl-bodge/graphics
                               cl-bodge/animation cl-bodge/text
                               cl-bodge/scenegraph bodge-sndfile
                               opticl cl-fad chipz flexi-streams log4cl)
  :pathname "assets/"
  :serial t
  :components ((:file "packages")
               (:file "resource-loader")
               (:file "audio")
               (:file "basic-chunks")
               (:file "image")
               (:file "font")
               (:file "encoded")
               (:file "converters")))


(defsystem cl-bodge/distribution
  :description "Bodacious Game Engine distribution helpers"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/utils cl-bodge/assets asdf uiop cl-fad cffi cl-ppcre
                              inferior-shell split-sequence flexi-streams)
  :pathname "distribution/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "distribution")
               (:file "registry")
               (:module darwin
                        :if-feature :darwin
                        :components ((:file "build")))
               (:module unix
                        :if-feature (:and :unix (:not :darwin))
                        :components ((:file "build")))
               (:module windows
                        :if-feature (:or :windows :win32)
                        :components ((:file "build")))
               (:file "build-unknown" :if-feature (:not (:or :darwin :unix :win32 :windows)))
               (:file "build")
               (:static-file "build.sh")))


(defsystem cl-bodge/tests
  :description "Test suite for cl-bodge engine"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/text cl-bodge/network cl-bodge/utils flexi-streams fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "suites")
               (:file "rated-queue")
               (:file "network")
               (:file "circular-buffer")
               (:file "buffered-output-stream")))


(defsystem cl-bodge
  :description "Bodacious Game Engine framework"
  :version "0.3.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (cl-bodge/engine cl-bodge/utils cl-bodge/host cl-bodge/network
                               cl-bodge/graphics cl-bodge/audio cl-bodge/physics
                               cl-bodge/resources cl-bodge/scenegraph
                               cl-bodge/poiu cl-bodge/text cl-bodge/canvas
                               cl-bodge/interactions cl-bodge/assets)
  :components ((:file "packages")))
