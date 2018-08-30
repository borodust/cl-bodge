# cl-bodge
Experimental **bod**acious **g**ame **e**ngine written in **C**ommon **L**isp.

1. [Documentation](#documentation)
1. [Engine modules](#engine-modules)
1. [Installation](#installation)
1. [Demonstrations](#demonstrations)
1. [Help and Support](#help-and-support)


## Documentation
See `cl-bodge` external [documentation](https://borodust.org/projects/cl-bodge/).

## Framework modules

* ***Foundations***

  Basic engine blocks for memory management, concurrency, math, events and systems. Everything
  else is built on top. See `cl-bodge/engine`.

* ***Host***

  Abstraction layer over host OS-dependent functionality: windows, OS resource
  management, system and input events, etc. See `cl-bodge/host`.

* ***Resources***

  Asset management routines for asynchronous resource loading, preparation, retrieving and
  releasing. See `cl-bodge/resources`.

* ***Graphics***

  OpenGL-driven rendering engine. Provides convenience layer over bare GL functionality: shared
  shader libraries and programs, extended shader preprocessor, vertex array objects, GPU
  buffers, framebuffers/renderbuffers, textures, etc. See `cl-bodge/graphics`.

* ***Canvas***

  Hardware-accelerated 2d drawing routines. See `cl-bodge/canvas`.

* ***Animation***

  Support module for skeletal animation: keyframe sequences, keyframe interpolation, skinning
  shader. See `cl-bodge/animation`.

* ***Audio***

  OpenAL-driven postional 3D audio system: listener, audio sources, audio buffers, streaming,
  special effects, etc. See `cl-bodge/audio`.

* ***Physics***

  2D and 3D rigid body physics engine with collision detection: rigid bodies, joints, shapes,
  etc. See `cl-bodge/physics`.

* ***Text***

  Text rendering system. Signed Distance Field based rendering, fonts handling, glyph aligning,
  font-related math, etc. See `cl-bodge/text`

* ***2D GUI***

  System for creating in-game hardware-accelerated multi-window user interfaces: windows,
  widgets, layouts.  See `cl-bodge/ui`.

* ***Distribution***

  Utilites for packaging application for shipping across different OSes: lisp image dumping,
  execution file creation, OS bundles, assets and foreign dependencies packing. See
  `cl-bodge/distribution`.

## Guides

* [Graphics](https://github.com/borodust/hello-bodge-graphics/blob/master/hello-bodge-graphics.org)
* [Physics](https://github.com/borodust/hello-bodge-physics/blob/master/hello-bodge-physics.org)

## Installation

### [Quicklisp](http://quicklisp.org)
```lisp
;; Add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

;; Update main dist just in case
(ql:update-dist "quicklisp")

;; Load the demo
(ql:quickload :cl-bodge/demo)
;; And run it!
(cl-bodge.demo:run)
```

## Misc

Also, check out [trivial-gamekit](https://github.com/borodust/trivial-gamekit) to get started
with `cl-bodge` bit by bit.

## Help and Support
You can receive those in `#lispgames` IRC channels at `freenode.net`.
