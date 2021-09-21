(cl:in-package :cl-bodge.graphics)


(defclass graphics-system (enableable dispatching generic-system)
  ((main-context :initform nil)
   (shared-contexts :initform nil))
  (:default-initargs :depends-on '(host-system)))


(defmethod enabling-flow list ((this graphics-system))
  (with-slots (main-context) this
    (>> (~> (for-host ()
              (framebuffer-size))
            (assembly-flow 'graphics-context :main t :system this))
        (instantly ((viewport context))
          (declare (type vec2 viewport))
          (setf main-context context)
          (update-context-framebuffer-size (floor (x viewport))
                                           (floor (y viewport))
                                           context)
          (subscribe 'framebuffer-size-change-event 'on-framebuffer-size-update)
          context)
        (%> (context)
          (flet ((init-glad ()
                   (unwind-protect
                        (progn
                          (log/debug "~%GL version: ~a~%GLSL version: ~a~%GL vendor: ~a~%GL renderer: ~a"
                                     (gl:get* :version)
                                     (gl:get* :shading-language-version)
                                     (gl:get* :vendor)
                                     (gl:get* :renderer))
                          (glad:init)
                          (log/debug "GLAD initialized"))
                     (run (concurrently () (continue-flow))))))
            (execute-with-context context #'init-glad))))))


(defmethod disabling-flow list ((this graphics-system))
  (with-slots (main-context shared-contexts) this
    (>> (instantly ()
          (unsubscribe 'framebuffer-size-change-event 'on-framebuffer-size-update))
        (-> this ()
          (clear-registry-cache))
        (loop for ctx in (append (list main-context) shared-contexts)
              collect (graphics-context-destructuring-flow ctx)))))


(defmacro for-graphics (&body arguments-and-body)
  "(for-graphics :context nil (&optional arg) &body body)"
  (multiple-value-bind (initargs arg-and-body)
      (parse-initargs-and-list arguments-and-body)
    `(-> (graphics) ,@initargs ,(first arg-and-body)
       ,@(rest arg-and-body))))


(defun on-framebuffer-size-update (event)
  (run (for-graphics ()
         (update-context-framebuffer-size (width-from event)
                                          (height-from event)))))


(defmethod dispatch ((this graphics-system) (task function) invariant &rest args
                     &key context disposing)
  (with-slots (main-context) this
    (let ((context (if (or (eq context nil)
                           (eq context t))
                       main-context
                       context)))
      (if disposing
          (apply #'execute-with-context context task :important-p t
                                                     :priority :highest
                                                     args)
          (apply #'execute-with-context context task args)))))


(defun register-shared-context (shared-context)
  (with-slots (shared-contexts) *system*
    (push shared-context shared-contexts))
  shared-context)


(defun graphics-context-assembly-flow ()
  (>> (->> ()
        (assembly-flow 'graphics-context :system (graphics)))
      (for-graphics (instance)
        (register-shared-context instance))))


(define-system-function reset-viewport graphics-system ()
  (reset-context-viewport))


(define-system-function viewport-width graphics-system ()
  (%framebuffer-width-of *graphics-context*))


(define-system-function viewport-height graphics-system ()
  (%framebuffer-height-of *graphics-context*))


(define-system-function graphics-context graphics-system ()
  *graphics-context*)


(definline graphics ()
  (engine-system 'graphics-system))


(define-system-function reset-state graphics-system ()
  (reset-context-state))


(defmacro preserving-state (&body body)
  `(with-state-slice ((preserve-state (%context-state *graphics-context*)))
     (unwind-protect
          (progn ,@body)
       (restore-state (%context-state *graphics-context*)))))


(defmacro in-wireframe-mode (&body body)
  `(unwind-protect
        (progn
          (gl:polygon-mode :front-and-back :line)
          ,@body)
     (gl:polygon-mode :front-and-back :fill)))
