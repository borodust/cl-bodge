(in-package :cl-bodge.poiu)


(defmacro dissect-c-struct ((&rest bindings) (wrapper type) &body body)
  (once-only (wrapper)
    (let ((let-bindings (loop for (var . access-list) in bindings collect
                             `(,var (c-ref ,wrapper (:struct (,type)) ,@access-list)))))
      `(symbol-macrolet ,let-bindings
         ,@body))))


(defun clamp (r g b a)
  (flet ((c (v)
           #f(min (max (/ v 255.0) 0.0) 1.0)))
    (vec4 (c r) (c g) (c b) (c a))))


(defun saturate (v max)
  #f(max (min v max) 0))


(defun draw-bounding-box (canvas x y w h r g b a)
  (draw-rect (vec2 #f x #f y) #f w #f h :stroke-color (clamp r g b a) :canvas canvas))


(defun render-scissor (cmd canvas)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
    (scissors (vec2 #f x #f y) w h canvas)))


(defun render-line (cmd canvas)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-line)
    (draw-line (vec2 #f x0 #f y0) (vec2 #f x1 #f y1) (clamp r g b a) :canvas canvas)))


(defun render-curve (cmd canvas)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (cx0 :ctrl 0 :x) (cy0 :ctrl 0 :y) (cx1 :ctrl 1 :x) (cy1 :ctrl 1 :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-curve)
    (draw-curve (vec2 #f x0 #f y0) (vec2 #f x1 #f y1)
                (vec2 #f cx0 #f cy0) (vec2 #f cx1 #f cy1)
                (clamp r g b a)
                :canvas canvas)))


(defun render-rect (cmd canvas)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickeness)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect)
    (draw-rect (vec2 #f x #f y) #f w #f h
               :stroke-color (clamp r g b a)
               :rounding #f rounding
               :canvas canvas)))


(defun render-rect-filled (cmd canvas)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect-filled)
    (draw-rect (vec2 #f x #f y) #f w #f h
               :fill-color (clamp r g b a)
               :rounding #f rounding
               :canvas canvas)))


(defun render-rect-multi-color (cmd canvas)
  (declare (ignore cmd canvas)))


(defun render-circle (cmd canvas)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickness)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 #f(+ x rx) #f(+ y ry)) #f rx #f ry
                    :stroke-color (clamp r g b a)
                    :thickness #f thickness
                    :canvas canvas))))


(defun render-circle-filled (cmd canvas)
 (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                    (r :color :r) (g :color :g) (b :color :b) (a :color :a))
     (cmd %nk:command-circle-filled)
   (let ((rx (/ w 2))
         (ry (/ h 2)))
     (draw-ellipse (vec2 #f(+ x rx) #f(+ y ry)) #f rx #f ry
                   :fill-color (clamp r g b a)
                   :canvas canvas))))


(defun render-arc (cmd canvas)
 (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                    (r :color :r) (g :color :g) (b :color :b) (a :color :a))
     (cmd %nk:command-arc)
   (draw-arc (vec2 #f x #f y) #f radius #f a0 #f a1
             :stroke-color (clamp r g b a)
             :canvas canvas)))


(defun render-arc-filled (cmd canvas)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc-filled)
    (draw-arc (vec2 #f x #f y) #f radius #f a0 #f a1
              :fill-color (clamp r g b a)
              :canvas canvas)))


(defun render-triangle (cmd canvas)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle)
    (draw-polygon (list (vec2 #f x0 #f y0)
                        (vec2 #f x1 #f y1)
                        (vec2 #f x2 #f y2))
                  :stroke-color (clamp r g b a)
                  :canvas canvas)))


(defun render-triangle-filled (cmd canvas)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle-filled)
    (draw-polygon (list (vec2 #f x0 #f y0)
                        (vec2 #f x1 #f y1)
                        (vec2 #f x2 #f y2))
                  :fill-color (clamp r g b a)
                  :canvas canvas)))


(defun render-polygon (cmd canvas)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :x)
                                     #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :y)))))
      (draw-polygon vertices :stroke-color (clamp r g b a)
                    :canvas canvas))))


(defun render-polygon-filled (cmd canvas)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon-filled)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :x)
                                     #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :y)))))
      (draw-polygon vertices :fill-color (clamp r g b a)
                    :canvas canvas))))


(defun render-polyline (cmd canvas)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polyline)
    (let ((points (loop for i from 0 below count
                     collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                   #f(c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polyline points (clamp r g b a)
                     :canvas canvas))))


(defun apply-pretext-scissors (cmd poiu)
  (let ((width (width-of poiu))
        (height (height-of poiu)))
    (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
      (gl:scissor (saturate x width) (saturate (- height y h) height)
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
                   :position (vec2 #f x #f y)
                   :color (clamp r g b a)))))


(defun render-clipped-text (cmd last-scissor poiu)
  (preserving-state
    (gx.state:enable :scissor-test)
    (gl:scissor 0.0 0.0 (width-of poiu) (height-of poiu))
    (gx.state:blend-func-separate :src-alpha :one-minus-src-alpha :zero :one)
    (when last-scissor
      (apply-pretext-scissors last-scissor poiu))
    (render-boxed-text cmd poiu)))


(defun render-text-bounding-box (cmd canvas)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (draw-bounding-box canvas x y w h r g b a)))


(defun render-image (cmd canvas)
  (declare (ignore cmd canvas)))


(defun command-type (cmd)
  (autowrap:enum-key '(:enum (%nk:command-type))
                     (c-ref cmd (:struct (%nk:command)) :type)))


(defmethod render ((poiu nuklear-context))
  (let ((canvas (canvas-of poiu))
        (width (floor (width-of poiu)))
        (height (floor (height-of poiu)))
        last-scissor)
    (unwind-protect
         (progn
           (begin-canvas canvas width height)
           (bodge-nuklear:docommands (cmd (handle-value-of poiu))
             (case (command-type cmd)
               (:nop)
               (:scissor (render-scissor cmd canvas) (setf last-scissor cmd))
               (:line (render-line cmd canvas))
               (:curve (render-curve cmd canvas))
               (:rect (render-rect cmd canvas))
               (:rect-filled (render-rect-filled cmd canvas))
               (:rect-multi-color (render-rect-multi-color cmd canvas))
               (:circle (render-circle cmd canvas))
               (:circle-filled (render-circle-filled cmd canvas))
               (:arc (render-arc cmd canvas))
               (:arc-filled (render-arc-filled cmd canvas))
               (:triangle (render-triangle cmd canvas))
               (:triangle-filled (render-triangle-filled cmd canvas))
               (:polygon (render-polygon cmd canvas))
               (:polygon-filled (render-polygon-filled cmd canvas))
               (:polyline (render-polyline cmd canvas))
               (:text (end-canvas canvas)
                      (render-clipped-text cmd last-scissor poiu)
                      (begin-canvas canvas width height))
               (:image (render-image cmd canvas)))))
      (end-canvas canvas)
      (reset-state))))


(defun render-poiu (poiu canvas)
  (with-complete-framebuffer ((framebuffer-of poiu) :color-buffers canvas)
    (preserving-state
      (gx.state:clear-color 0.0 0.0 0.0 0.0)
      (gx.state:clear-depth 1.0)
      (gl:clear :color-buffer :depth-buffer :stencil-buffer)

      (render poiu))))
