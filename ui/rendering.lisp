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


(defun saturate (v max)
  (max (min v max) 0))


(definline %invert (y ctx &optional (h 0.0))
  (- (height-of ctx) y h))


(defun draw-bounding-box (ui x y w h &optional (r 255) (g 0) (b 0) (a 255))
  (draw-rect (vec2 x (%invert y ui h)) w h
             :stroke-paint (clamp r g b a)
             :canvas (canvas-of ui)))


(defun render-scissor (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
    (scissors (vec2 x (%invert y ui h)) w h (canvas-of ui))))


(defun render-line (cmd ui)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-line)
    (draw-line (vec2 x0 (%invert y0 ui)) (vec2 x1 (%invert y1 ui)) (clamp r g b a)
               :canvas (canvas-of ui))))


(defun render-curve (cmd ui)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (cx0 :ctrl 0 :x) (cy0 :ctrl 0 :y) (cx1 :ctrl 1 :x) (cy1 :ctrl 1 :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-curve)
    (draw-curve (vec2 x0 (%invert y0 ui)) (vec2 x1 (%invert y1 ui))
                (vec2 cx0 cy0) (vec2 cx1 cy1)
                (clamp r g b a)
                :canvas (canvas-of ui))))


(defun render-rect (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickeness)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect)
    (draw-rect (vec2 x (%invert y ui h)) w h
               :stroke-paint (clamp r g b a)
               :rounding rounding
               :canvas (canvas-of ui))))


(defun render-rect-filled (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect-filled)
    (draw-rect (vec2 x (%invert y ui h)) w h
               :fill-paint (clamp r g b a)
               :rounding rounding
               :canvas (canvas-of ui))))


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
      (with-font ((font-of ui) (canvas-of ui))
        (draw-text (vec2 x (%invert y ui (canvas-font-ascender
                                            (canvas-font-metrics (font-of ui)
                                                                 (canvas-of ui)))))
                   lisp-string
                   :fill-color (clamp r g b a))))))


(defun render-text-bounding-box (cmd ui)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (draw-bounding-box ui x y w h r g b a)))


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
    (clear-ui-context)
    (loop for win in (%windows-of context)
          do (compose win))
    (render-ui)))
