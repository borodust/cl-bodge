(cl:in-package :cl-bodge.ui)


(defun clamp (r g b a)
  (flet ((c (v)
           (min (max (/ v 255.0) 0.0) 1.0)))
    (vec4 (c r) (c g) (c b) (c a))))


(defun bodge-color (nk-color)
  (c-val ((nk-color (:struct (%nk:color))))
    (clamp (nk-color :r) (nk-color :g) (nk-color :b) (nk-color :a))))


(definline %invert (y ctx &optional (h 0.0))
  (- (height-of ctx) y h))


(defmacro as-command ((cmd-var type) &body body)
  `(claw:c-val ((,cmd-var (:struct (,type))))
     ,@body))


(defun render-scissor (cmd ui)
  (as-command (cmd %nk:command-scissor)
    (scissors (vec2 (cmd :x) (%invert (cmd :y) ui (cmd :h)))
              (cmd :w) (cmd :h)
              (canvas-of ui))))


(defun render-line (cmd ui)
  (as-command (cmd %nk:command-line)
    (let ((x0 (cmd :begin :x))
          (y0 (cmd :begin :y))
          (x1 (cmd :end :x))
          (y1 (cmd :end :y)))
      (draw-line (vec2 x0 (%invert y0 ui)) (vec2 x1 (%invert y1 ui))
                 (bodge-color (cmd :color))
                 :thickness (cmd :line-thickness)
                 :canvas (canvas-of ui)))))


(defun render-curve (cmd ui)
  (as-command (cmd %nk:command-curve)
    (let ((x0 (cmd :begin :x))
          (y0 (cmd :begin :y))
          (x1 (cmd :end :x))
          (y1 (cmd :end :y))
          (cx0 (cmd :ctrl 0 :x))
          (cy0 (cmd :ctrl 0 :y))
          (cx1 (cmd :ctrl 1 :x))
          (cy1 (cmd :ctrl 1 :y)))
      (draw-curve (vec2 x0 (%invert y0 ui)) (vec2 x1 (%invert y1 ui))
                  (vec2 cx0 cy0) (vec2 cx1 cy1)
                  (bodge-color (cmd :color))
                  :thickness (cmd :line-thickness)
                  :canvas (canvas-of ui)))))


(defun render-rect (cmd ui)
  (as-command (cmd %nk:command-rect)
    (let ((x (cmd :x)) (y (cmd :y))
          (w (cmd :w)) (h (cmd :h))
          (thickness (cmd :line-thickness))
          (rounding (cmd :rounding)))
      (draw-rect (vec2 x (%invert y ui h)) w h
                 :stroke-paint (bodge-color (cmd :color))
                 :rounding rounding
                 :thickness thickness
                 :canvas (canvas-of ui)))))


(defun render-rect-filled (cmd ui)
  (as-command (cmd %nk:command-rect-filled)
    (let ((x (cmd :x))
          (y (cmd :y))
          (w (cmd :w))
          (h (cmd :h))
          (rounding (cmd :rounding)))
      (draw-rect (vec2 x (%invert y ui h)) w h
                 :fill-paint (bodge-color (cmd :color))
                 :rounding rounding
                 :canvas (canvas-of ui)))))


(defun render-rect-multi-color (cmd ui)
  (declare (ignore cmd ui)))


(defun render-circle (cmd ui)
  (as-command (cmd %nk:command-circle)
    (let* ((x (cmd :x))
           (y (cmd :y))
           (w (cmd :w))
           (h (cmd :h))
           (rx (/ w 2))
           (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y ui h) ry)) rx ry
                    :stroke-paint (bodge-color (cmd :color))
                    :thickness (cmd :line-thickness)
                    :canvas (canvas-of ui)))))


(defun render-circle-filled (cmd ui)
  (as-command (cmd %nk:command-circle-filled)
    (let* ((x (cmd :x))
           (y (cmd :y))
           (w (cmd :w))
           (h (cmd :h))
           (rx (/ w 2))
           (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y ui h) ry)) rx ry
                    :fill-paint (bodge-color (cmd :color))
                    :canvas (canvas-of ui)))))


(defun render-arc (cmd ui)
  (as-command (cmd %nk:command-arc)
    (let ((x (cmd :cx))
          (y (cmd :cy))
          (radius (cmd :r))
          (a0 (cmd :a 0))
          (a1 (cmd :a 1)))
      (draw-arc (vec2 x (%invert y ui)) radius a0 a1
                :stroke-paint (bodge-color (cmd :color))
                :thickness (cmd :line-thickness)
                :canvas (canvas-of ui)))))


(defun render-arc-filled (cmd ui)
  (as-command (cmd %nk:command-arc-filled)
    (let ((x (cmd :cx))
          (y (cmd :cy))
          (radius (cmd :r))
          (a0 (cmd :a 0))
          (a1 (cmd :a 1)))
      (draw-arc (vec2 x (%invert y ui)) radius a0 a1
                :fill-paint (bodge-color (cmd :color))
                :canvas (canvas-of ui)))))


(defun render-triangle (cmd ui)
  (as-command (cmd %nk:command-triangle)
    (let ((x0 (cmd :a :x))
          (y0 (cmd :a :y))
          (x1 (cmd :b :x))
          (y1 (cmd :b :y))
          (x2 (cmd :c :x))
          (y2 (cmd :c :y)))
      (draw-polygon (list (vec2 x0 (%invert y0 ui))
                          (vec2 x1 (%invert y1 ui))
                          (vec2 x2 y2))
                    :stroke-paint (bodge-color (cmd :color))
                    :thickness (cmd :line-thickness)
                    :canvas (canvas-of ui)))))


(defun render-triangle-filled (cmd ui)
  (as-command (cmd %nk:command-triangle-filled)
    (let ((x0 (cmd :a :x))
          (y0 (cmd :a :y))
          (x1 (cmd :b :x))
          (y1 (cmd :b :y))
          (x2 (cmd :c :x))
          (y2 (cmd :c :y)))
      (draw-polygon (list (vec2 x0 (%invert y0 ui))
                          (vec2 x1 (%invert y1 ui))
                          (vec2 x2 (%invert y2 ui)))
                    :fill-paint (bodge-color (cmd :color))
                    :canvas (canvas-of ui)))))


(defun render-polygon (cmd ui)
  (as-command (cmd %nk:command-polygon)
    (let* ((count (cmd :point-count))
           (vertices (loop for i from 0 below count
                           collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                                :points i :x)
                                         (c-ref cmd (:struct (%nk:command-polygon))
                                                :points i :y)))))
      (draw-polygon vertices
                    :stroke-paint (bodge-color (cmd :color))
                    :thickness (cmd :line-thickness)
                    :canvas (canvas-of ui)))))


(defun render-polygon-filled (cmd ui)
  (as-command (cmd %nk:command-polygon-filled)
    (let* ((count (cmd :point-count))
           (vertices (loop for i from 0 below count
                           collect (vec2 (c-ref cmd (:struct (%nk:command-polygon-filled))
                                                :points i :x)
                                         (c-ref cmd (:struct (%nk:command-polygon-filled))
                                                :points i :y)))))
      (draw-polygon vertices
                    :fill-paint (bodge-color (cmd :color))
                    :canvas (canvas-of ui)))))


(defun render-polyline (cmd ui)
  (as-command (cmd %nk:command-polyline)
    (let* ((count (cmd :point-count))
           (points (loop for i from 0 below count
                         collect (vec2 (c-ref cmd (:struct (%nk:command-polyline))
                                              :points i :x)
                                       (c-ref cmd (:struct (%nk:command-polyline))
                                              :points i :y)))))
      (draw-polyline points (bodge-color (cmd :color))
                     :thickness (cmd :line-thickness)
                     :canvas (canvas-of ui)))))


(defun render-text (cmd ui)
  (as-command (cmd %nk:command-text)
    (let* ((x (cmd :x))
           (y (cmd :y))
           (lisp-string (cffi:foreign-string-to-lisp (cmd :string &) :count (cmd :length))))
      (draw-text (vec2 x (%invert y ui (canvas-font-ascender
                                        (canvas-font-metrics (canvas-of ui)))))
                 lisp-string
                 :fill-color (bodge-color (cmd :foreground))))))


(defun render-image (cmd ui)
  (declare (ignore cmd ui)))


(defun render-custom (cmd ui)
  (as-command (cmd %nk:command-custom)
    (when-let ((widget (context-custom-widget (cffi:pointer-address (cmd :callback-data :ptr)) ui)))
      (let ((height (cmd :h)))
        (render-custom-widget widget (vec2 (cmd :x) (%invert (cmd :y) ui height)) (cmd :w) height)))))


(defun command-type (cmd)
  (claw:enum-key '(:enum (%nk:command-type)) (c-ref cmd (:struct (%nk:command)) :type)))


(defun render-ui (&optional (ui *context*))
  (with-canvas ((canvas-of ui))
    (nuklear:docommands (cmd (handle-value-of ui))
      (case (command-type cmd)
        (:nop)
        (:scissor (render-scissor cmd ui))
        (:line (render-line cmd ui))
        (:curve (render-curve cmd ui))
        (:rect (render-rect cmd ui))
        (:rect-filled (render-rect-filled cmd ui))
        (:rect-multi-color (render-rect-multi-color cmd ui))
        (:circle (render-circle cmd ui))
        (:circle-filled (render-circle-filled cmd ui))
        (:arc (render-arc cmd ui))
        (:arc-filled (render-arc-filled cmd ui))
        (:triangle (render-triangle cmd ui))
        (:triangle-filled (render-triangle-filled cmd ui))
        (:polygon (render-polygon cmd ui))
        (:polygon-filled (render-polygon-filled cmd ui))
        (:polyline (render-polyline cmd ui))
        (:text (render-text cmd ui))
        (:image (render-image cmd ui))
        (:custom (render-custom cmd ui))))))


(define-system-function compose-ui graphics-system (context)
  (with-ui (context)
    (clear-ui)
    (drain-compose-task-queue context)
    (when-let ((input-source (%input-source-of context)))
      (with-ui-input (context)
        (loop (multiple-value-bind (key state) (next-keyboard-interaction input-source)
                (if key
                    (register-keyboard-input key state)
                    (return))))
        (let ((cursor (last-cursor-position input-source
                                            (%last-cursor-position-of context))))
          (loop (multiple-value-bind (button state) (next-mouse-interaction input-source)
                  (if button
                      (register-mouse-input (x cursor) (y cursor) button state))
                      (return)))
          (register-cursor-position (x cursor) (y cursor)))
        (loop for character = (next-character input-source)
              while character
              do (register-character-input character))
        (let ((scroll (next-scroll input-source (%last-scroll-of context))))
          (register-scroll-input (x scroll) (y scroll)))))
    (loop for win in (%windows-of context)
          do (compose win))
    (render-ui)))
