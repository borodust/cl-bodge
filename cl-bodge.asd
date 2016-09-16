(in-package :cl-user)

(defpackage :cl-bodge.package
  (:use :cl :asdf))

(in-package :cl-bodge.package)

(defsystem cl-bodge
  :description "Bodacious Game Engine"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:cl-opengl :cl-glfw3 :log4cl :bordeaux-threads :trivial-main-thread)
  :serial t
  :components ((:file "packages")
               (:file "bootstrap")))
