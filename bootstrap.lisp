(in-package :cl-bodge.bootstrap)

(defvar *engine* nil)

(defun %render-to (window)
  (glfw:make-context-current window)
  (loop until (glfw:window-should-close-p window) do
       (sleep 0.01)
       (glfw:swap-buffers))
  (log:info "Rendering thread exiting"))

(defstruct engine
  (main-window nil :read-only t))

(defun %free-engine-resources ()
  (setf *engine* nil))

(glfw:def-window-close-callback on-window-close (win)
  (declare (ignore win))
  (%free-engine-resources))

(defun startup ()
  (if (not (null *engine*))
      (error "Engine already started")
      (with-body-in-main-thread ()
        (handler-case 
            (glfw:with-init-window (:title "Rendering output" :width 640 :height 480
                                           :context-version-major 4
                                           :context-version-minor 1
                                           :opengl-profile :opengl-core-profile
                                           :opengl-forward-compat t
                                           :samples 4)
              (log:info "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a~%"
                        (gl:get* :version)
                        (gl:get* :shading-language-version)
                        (gl:get* :vendor)
                        (gl:get* :renderer))

              (glfw:set-window-close-callback 'on-window-close)

              (let* ((win glfw:*window*)
                     (rendering-thread (bt:make-thread (lambda () (%render-to win))
                                                       :name "rendering-thread")))
                (setf *engine* (make-engine :main-window win))
                (loop until (glfw:window-should-close-p) do
                     (glfw:wait-events))
                (bt:join-thread rendering-thread)
                (log:info "Main thread exiting")))
          (t (e) (log:error "Unhandled error: ~a" e))))))

(defun shutdown ()
  (cond
    ((null *engine*) (error "Engine is already offline"))
    (t (glfw:set-window-should-close (engine-main-window *engine*))
       (glfw:post-empty-event))))
