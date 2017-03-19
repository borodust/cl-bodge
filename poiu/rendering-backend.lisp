(in-package :cl-bodge.poiu)


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


(defun draw-bounding-box (poiu x y w h &optional (r 255) (g 0) (b 0) (a 255))
  (draw-rect (vec2 x (%invert y poiu h)) w h
             :stroke-color (clamp r g b a)
             :canvas (canvas-of poiu)))


(defun render-scissor (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
    (scissors (vec2 x (%invert y poiu h)) w h (canvas-of poiu))))


(defun render-line (cmd poiu)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-line)
    (draw-line (vec2 x0 (%invert y0 poiu)) (vec2 x1 (%invert y1 poiu)) (clamp r g b a)
               :canvas (canvas-of poiu))))


(defun render-curve (cmd poiu)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (cx0 :ctrl 0 :x) (cy0 :ctrl 0 :y) (cx1 :ctrl 1 :x) (cy1 :ctrl 1 :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-curve)
    (draw-curve (vec2 x0 (%invert y0 poiu)) (vec2 x1 (%invert y1 poiu))
                (vec2 cx0 cy0) (vec2 cx1 cy1)
                (clamp r g b a)
                :canvas (canvas-of poiu))))


(defun render-rect (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickeness)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect)
    (draw-rect (vec2 x (%invert y poiu h)) w h
               :stroke-color (clamp r g b a)
               :rounding rounding
               :canvas (canvas-of poiu))))


(defun render-rect-filled (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect-filled)
    (draw-rect (vec2 x (%invert y poiu h)) w h
               :fill-color (clamp r g b a)
               :rounding rounding
               :canvas (canvas-of poiu))))


(defun render-rect-multi-color (cmd poiu)
  (declare (ignore cmd poiu)))


(defun render-circle (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickness)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y poiu) ry)) rx ry
                    :stroke-color (clamp r g b a)
                    :thickness thickness
                    :canvas (canvas-of poiu)))))


(defun render-circle-filled (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle-filled)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 (+ x rx) (+ (%invert y poiu) ry)) rx ry
                    :fill-color (clamp r g b a)
                    :canvas (canvas-of poiu)))))


(defun render-arc (cmd poiu)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc)
    (draw-arc (vec2 x (%invert y poiu)) radius a0 a1
              :stroke-color (clamp r g b a)
              :canvas (canvas-of poiu))))


(defun render-arc-filled (cmd poiu)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc-filled)
    (draw-arc (vec2 x (%invert y poiu)) radius a0 a1
              :fill-color (clamp r g b a)
              :canvas (canvas-of poiu))))


(defun render-triangle (cmd poiu)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle)
    (draw-polygon (list (vec2 x0 (%invert y0 poiu))
                        (vec2 x1 (%invert y1 poiu))
                        (vec2 x2 y2))
                  :stroke-color (clamp r g b a)
                  :canvas (canvas-of poiu))))


(defun render-triangle-filled (cmd poiu)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle-filled)
    (draw-polygon (list (vec2 x0 (%invert y0 poiu))
                        (vec2 x1 (%invert y1 poiu))
                        (vec2 x2 (%invert y2 poiu)))
                  :fill-color (clamp r g b a)
                  :canvas (canvas-of poiu))))


(defun render-polygon (cmd poiu)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                     (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polygon vertices :stroke-color (clamp r g b a)
                    :canvas (canvas-of poiu)))))


(defun render-polygon-filled (cmd poiu)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon-filled)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                     (c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polygon vertices :fill-color (clamp r g b a)
                    :canvas (canvas-of poiu)))))


(defun render-polyline (cmd poiu)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polyline)
    (let ((points (loop for i from 0 below count
                     collect (vec2 (c-ref cmd (:struct (%nk:command-polygon))
                                          :points i :x)
                                   (c-ref cmd (:struct (%nk:command-polygon))
                                          :points i :y)))))
      (draw-polyline points (clamp r g b a)
                     :canvas (canvas-of poiu)))))


(defun apply-pretext-scissors (cmd poiu)
  (let ((width (width-of poiu))
        (height (height-of poiu)))
    (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
      (gl:scissor (saturate x width) (%invert (saturate y height) poiu h)
                  (saturate w width) (saturate h height)))))


(defun render-boxed-text (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (length :length)
                     (height :height)
                     (string :string &)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (let ((lisp-string (cffi:foreign-string-to-lisp string :count length)))
      (draw-text (text-renderer-of poiu) lisp-string
                 :position (vec2 x (%invert y poiu (text-line-height (text-renderer-of poiu))))
                 :color (clamp r g b a)))))


(defun render-clipped-text (cmd last-scissor poiu)
  (preserving-state
    (gx.state:enable :scissor-test)
    (gx.state:blend-func-separate :src-alpha :one-minus-src-alpha :zero :one)
    (when last-scissor
      (apply-pretext-scissors last-scissor poiu))
    (render-boxed-text cmd poiu)
    (gl:scissor 0.0 0.0 (width-of poiu) (height-of poiu))))


(defun render-text-bounding-box (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (draw-bounding-box poiu x y w h r g b a)))


(defun render-image (cmd poiu)
  (declare (ignore cmd poiu)))


(defun command-type (cmd)
  (autowrap:enum-key '(:enum (%nk:command-type))
                     (c-ref cmd (:struct (%nk:command)) :type)))


(defmethod render ((poiu nuklear-context))
  (let ((canvas (canvas-of poiu))
        prev-command)
    (unwind-protect
         (progn
           (begin-canvas canvas)
           (bodge-nuklear:docommands (cmd (handle-value-of poiu))
             (case (command-type cmd)
               (:nop)
               (:scissor (render-scissor cmd poiu))
               (:line (render-line cmd poiu))
               (:curve (render-curve cmd poiu))
               (:rect (render-rect cmd poiu))
               (:rect-filled (render-rect-filled cmd poiu))
               (:rect-multi-color (render-rect-multi-color cmd poiu))
               (:circle (render-circle cmd poiu))
               (:circle-filled (render-circle-filled cmd poiu))
               (:arc (render-arc cmd poiu))
               (:arc-filled (render-arc-filled cmd poiu))
               (:triangle (render-triangle cmd poiu))
               (:triangle-filled (render-triangle-filled cmd poiu))
               (:polygon (render-polygon cmd poiu))
               (:polygon-filled (render-polygon-filled cmd poiu))
               (:polyline (render-polyline cmd poiu))
               (:text (end-canvas (canvas-of poiu))
                      (let ((scissor-cmd (when (and prev-command
                                                    (eq :scissor (command-type prev-command)))
                                           prev-command)))
                        (render-clipped-text cmd scissor-cmd poiu))
                      (begin-canvas canvas))
               (:image (render-image cmd poiu)))
             (setf prev-command cmd)))
      (end-canvas canvas)
      (reset-state))))
