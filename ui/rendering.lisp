(cl:in-package :cl-bodge.ui)


(defmacro dissect-c-struct ((&rest bindings) (wrapper type) &body body)
  (once-only (wrapper)
    (let ((let-bindings (loop for (var . access-list) in bindings collect
                             `(,var (c-ref ,wrapper (:struct (,type)) ,@access-list)))))
      `(symbol-macrolet ,let-bindings
         ,@body))))


(defun clamp (r g b a)
  (flet ((c (v)
           (min (max (/ v 255.0) 0.0) 1.0)))
    (vec4 (c r) (c g) (c b) (c a))))


(defun bodge-color (nk-color)
  (c-val ((nk-color (:struct (%nk:color))))
    (clamp (nk-color :r) (nk-color :g) (nk-color :b) (nk-color :a))))


(definline saturate (v max)
  (max (min v max) 0))


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
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickness)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y ui) ry)) rx ry
                    :stroke-paint (clamp r g b a)
                    :thickness thickness
                    :canvas (canvas-of ui)))))


(defun render-circle-filled (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle-filled)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y ui) ry)) rx ry
                    :fill-paint (clamp r g b a)
                    :canvas (canvas-of ui)))))


(defun render-arc (cmd ui)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc)
    (draw-arc (vec2 x (%invert y ui)) radius a0 a1
              :stroke-paint (clamp r g b a)
              :canvas (canvas-of ui))))


(defun render-arc-filled (cmd ui)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc-filled)
    (draw-arc (vec2 x (%invert y ui)) radius a0 a1
              :fill-paint (clamp r g b a)
              :canvas (canvas-of ui))))


(defun render-triangle (cmd ui)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle)
    (draw-polygon (list (vec2 x0 (%invert y0 ui))
                        (vec2 x1 (%invert y1 ui))
                        (vec2 x2 y2))
                  :stroke-paint (clamp r g b a)
                  :canvas (canvas-of ui))))


(defun render-triangle-filled (cmd ui)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle-filled)
    (draw-polygon (list (vec2 x0 (%invert y0 ui))
                        (vec2 x1 (%invert y1 ui))
                        (vec2 x2 (%invert y2 ui)))
                  :fill-paint (clamp r g b a)
                  :canvas (canvas-of ui))))


(defun render-polygon (cmd ui)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                     (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polygon vertices :stroke-paint (clamp r g b a)
                    :canvas (canvas-of ui)))))


(defun render-polygon-filled (cmd ui)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon-filled)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                     (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polygon vertices :fill-paint (clamp r g b a)
                    :canvas (canvas-of ui)))))


(defun render-polyline (cmd ui)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polyline)
    (let ((points (loop for i from 0 below count
                     collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                          :points i :x)
                                   (c-ref cmd (:struct (%nk:command-polygon))
                                          :points i :y)))))
      (draw-polyline points (clamp r g b a)
                     :canvas (canvas-of ui)))))


(defun apply-pretext-scissors (cmd ui)
  (let ((width (width-of ui))
        (height (height-of ui)))
    (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
      (gl:scissor (saturate x width) (%invert (saturate y height) ui h)
                  (saturate w width) (saturate h height)))))


(defun render-text (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (length :length)
                     (height :height)
                     (string :string &)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (let ((lisp-string (cffi:foreign-string-to-lisp string :count length)))
      (draw-text (vec2 x (%invert y ui (canvas-font-ascender
                                        (canvas-font-metrics (canvas-of ui)))))
                 lisp-string
                 :fill-color (clamp r g b a)))))


(defun render-image (cmd ui)
  (declare (ignore cmd ui)))


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
        (:image (render-image cmd ui))))))


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
