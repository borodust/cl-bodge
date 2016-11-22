# cl-bodge
Bodacious 3D game engine that is no good. Yet.

Use [anything else](https://github.com/lispgames/lispgames.github.io/wiki/CommonLisp) instead.

## Installation

### Quicklisp
No official quicklisp release is planned due to the nature of the engine being overloaded with bunch of foreign libraries, albeit possibility of dedicated quicklisp repository with necessary dependencies in it is still kept in view.

### From sources

Required foreign libraries:
* `libffi`     >= 3.0
* `glfw3`      >= 3.1
* `OpenAL`     >= 1.1
* `ODE`        >= 0.14
* `libsndfile` >= 1.0
* `OpenGL`     >= 4.1

You need few dependencies manually installed (no quicklisp packages either):
* [`cl-muth`](https://github.com/borodust/cl-muth)
* [`bodge-ode`](https://github.com/borodust/bodge-ode)
* [`bodge-sndfile`](https://github.com/borodust/bodge-sndfile)

Finally, you need to clone this repository to your local machine and setup quicklisp, so it could find engine's source code.


After all foreign libraries are installed to respective system default paths, engine and its lisp dependencies are made available to quicklisp you should be able to load it with
```lisp
(ql:quickload :cl-bodge)
```

## Help and Support
You can receive those in `#cl-bodge` IRC channel at `freenode.net`.