# cl-bodge
Experimental **bod**acious **g**ame **e**ngine written in **C**ommon **L**isp.

1. [Engine modules](#engine-modules)
1. [Installation](#installation)
1. [Demonstrations](#demonstrations)
1. [Help and Support](#help-and-support)


## Engine modules

* ***Foundations***

  Basic engine blocks for memory management, concurrency, math and systems. Everything else is
  built on top. See `cl-bodge/engine`.

* ***Events***

  Base for event-driven systems. See `cl-bodge/events`.

* ***Host***

  Abstraction layer over host OS-dependent functionality: windows, OS resource
  management, system and input events, etc. See `cl-bodge/host`.

* ***Assets***

  Asset management routines for asynchronous resource loading, preparation, retrieving and
  releasing. See `cl-bodge/assets`.

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

  ODE-backed rigid body physics engine with collision detection: rigid bodies, joints, geoms,
  spaces (geom islands), etc. See `cl-bodge/physics`.

* ***Text***

  Text rendering system. Signed Distance Field based rendering, fonts handling, glyph aligning,
  font-related math, etc. See `cl-bodge/text`

* ***2D GUI***

  System for creating in-game hardware-accelerated multi-window user interfaces: windows,
  widgets, layouts, UI events.  See `cl-bodge/poiu` (**P**lain **O**ld **I**nterface for
  **U**sers).

* ***Scenegraph***

  Node-based scene processing. High-level convenience layer on top of low-level systems: scene
  passes (rendering, simulation, etc); graphics-oriented, physics-oriented, transformation,
  animation, generic model and other types of nodes. See `cl-bodge/scenegraph`.

* ***Resources***

  Engine's universal Bodge Resource File and various external formats parsing and loading: images,
  audio, fonts, meshes, skeletons, animations, etc. See `cl-bodge/resources`

* ***Distribution***

  Utilites for packaging application for shipping across different OSes: lisp image dumping,
  execution file creation, OS bundles, assets and foreign dependencies packing. See
  `cl-bodge/distribution`.


## Installation

### [Quicklisp](http://quicklisp.org)
```lisp
;; add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/bodge-systems.txt")

;; load precompiled native libraries and the engine
(ql:quickload '(:bodge-blobs :cl-bodge))
```

### Upstream sources

Required foreign libraries:

| Library | Version | Dependent system
|---------|:-------:|-------------------------
| `libffi`     | 3.0  | `cl-bodge/host`
| `glfw3`      | 3.1  | `cl-bodge/host`
| `OpenAL`     | 1.1  | `cl-bodge/audio`
| `ODE`        | 0.14 | `cl-bodge/physics`
| `libsndfile` | 1.0  | `cl-bodge/resources`
| `OpenGL`     | 4.1  | `cl-bodge/graphics`
| `libepoxy`   | ?.?  | `bodge-nanovg`
| `NanoVG`     | ?.?  | `bodge-nanovg`
| `Nuklear`    | 1.20 | `bodge-nuklear`


You need few CL dependencies installed manually:
* [`cl-muth`](https://github.com/borodust/cl-muth)
* [`cl-flow`](https://github.com/borodust/cl-flow)
* [`bodge-ode`](https://github.com/borodust/bodge-ode)
* [`bodge-sndfile`](https://github.com/borodust/bodge-sndfile)
* [`bodge-nuklear`](https://github.com/borodust/bodge-nuklear)
* [`bodge-nanovg`](https://github.com/borodust/bodge-nanovg)

Finally, you need to clone this repository to your local machine and setup
[Quicklisp](https://www.quicklisp.org/), so it could find engine's source code.

For instructions on how to setup local projects, please, refer to [Quicklisp
FAQ](https://www.quicklisp.org/beta/faq.html#local-project)


After all foreign libraries are installed to respective system default paths, engine and lisp
dependencies are made available to Quicklisp you should be able to load engine with

```lisp
(ql:quickload :cl-bodge)
```

## Demonstrations

* Chicken mesh loading, rendering and animation:
  [Chicken](https://www.youtube.com/watch?v=ypZP4SNQOv8)

* Ball-Z game written for
  [Autumn 2016 Lisp Game Jam](https://itch.io/jam/autumn-2016-lisp-game-jam/rate/99353):
  [Ball-Z](https://www.youtube.com/watch?v=noVtO2H9hSY)

* SDF-based text rendering: [Hello text](https://www.youtube.com/watch?v=8q_ssF4eEQQ)
* GUI: [Multi-window GUI](https://www.youtube.com/watch?v=eLFMUCvjEXg),
  [Text editing](https://www.youtube.com/watch?v=T5nCKKGj1J0)

## Help and Support
You can receive those in `#cl-bodge` or `#lispgames` IRC channels at `freenode.net`.