(in-package :cl-bodge.application)

(defclass application-system (system)
  ((enabled-p :initform nil)
   (state-lock :initform (make-lock "app-sys-state-lock"))
   (state-condi-var :initform (make-condition-variable
                               :name "app-sys-state-condi-var"))
   (window :initform nil)
   (job-queue :initform (make-job-queue))))

(defmacro within-main-thread-of ((app-system) &body body)
  (with-gensyms (queue)
    `(with-slots ((,queue job-queue)) ,app-system
       (push-body-into (,queue)
         ,@body)
       (glfw:post-empty-event))))


(glfw:def-window-close-callback on-close (window)
  (glfw:hide-window window))
  

;; if current thread is the main one, this function will block
(defmethod enable ((this application-system))
  (with-slots (enabled-p job-queue window state-lock state-condi-var) this
    (with-lock-held (state-lock)
      (when enabled-p
        (error "Application system already enabled"))
      (with-body-in-main-thread ()
        (log-errors
          (glfw:with-init-window (:title "Scene" :width 640 :height 480
                                         :context-version-major 4
                                         :context-version-minor 1
                                         :opengl-profile :opengl-core-profile
                                         :opengl-forward-compat t
                                         :samples 4)
            (glfw:set-window-close-callback 'on-close)
            (with-lock-held (state-lock)
              (setf window glfw:*window*
                    enabled-p t))
            (condition-notify state-condi-var)
            (log:info "Application main loop running")
            (let ((*application* this))
              (declare (special *application*))
              (loop while enabled-p
                 do (handler-case
                        (progn
                          (glfw:wait-events)
                          (drain job-queue))
                      (t (e) (log:error "Unhandled error in the event-loop: ~a" e))))
              (condition-notify state-condi-var)))
          (log:info "Main loop stopped. Application system offline")))
      (loop until enabled-p do
           (condition-wait state-condi-var state-lock)))))



(defmethod disable ((this application-system))
  (with-slots (enabled-p state-lock state-condi-var) this
    (with-lock-held (state-lock)
      (unless enabled-p
        (error "Application system already disabled"))
      (within-main-thread-of (this)
        (with-slots (enabled-p) this
          (with-lock-held (state-lock)
            (setf enabled-p nil))))
      (loop while enabled-p do
           (condition-wait state-condi-var state-lock)))))


(defun bind-rendering-context (app-sys)
  (with-slots (window) app-sys
    (glfw:make-context-current window)))


(defun swap-buffers (app-sys)
  (with-slots (window) app-sys
    (glfw:swap-buffers window)))
