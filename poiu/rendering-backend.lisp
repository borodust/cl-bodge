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


(defun draw-bounding-box (x y w h r g b a)
  (draw-rect (vec2 #f x #f y) #f w #f h :stroke-color (clamp r g b a)))


(defun render-scissor (cmd)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)) (cmd %nk:command-scissor)
    (scissors (vec2 #f x #f y) w h)))


(defun render-line (cmd)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-line)
    (draw-line (vec2 #f x0 #f y0) (vec2 #f x1 #f y1) (clamp r g b a))))


(defun render-curve (cmd)
  (dissect-c-struct ((x0 :begin :x) (y0 :begin :y) (x1 :end :x) (y1 :end :y)
                     (cx0 :ctrl 0 :x) (cy0 :ctrl 0 :y) (cx1 :ctrl 1 :x) (cy1 :ctrl 1 :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-curve)
    (draw-curve (vec2 #f x0 #f y0) (vec2 #f x1 #f y1)
                (vec2 #f cx0 #f cy0) (vec2 #f cx1 #f cy1)
                (clamp r g b a))))


(defun render-rect (cmd)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickeness)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect)
    (draw-rect (vec2 #f x #f y) #f w #f h
               :stroke-color (clamp r g b a)
               :rounding #f rounding)))


(defun render-rect-filled (cmd)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (rounding :rounding)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-rect-filled)
    (draw-rect (vec2 #f x #f y) #f w #f h
               :fill-color (clamp r g b a)
               :rounding #f rounding)))


(defun render-rect-multi-color (cmd)
  (declare (ignore cmd)))


(defun render-circle (cmd)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (thickness :line-thickness)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-circle)
    (let ((rx (/ w 2))
          (ry (/ h 2)))
      (draw-ellipse (vec2 #f(+ x rx) #f(+ y ry)) #f rx #f ry
                    :stroke-color (clamp r g b a)
                    :thickness #f thickness))))


(defun render-circle-filled (cmd)
 (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                    (r :color :r) (g :color :g) (b :color :b) (a :color :a))
     (cmd %nk:command-circle-filled)
   (let ((rx (/ w 2))
         (ry (/ h 2)))
     (draw-ellipse (vec2 #f(+ x rx) #f(+ y ry)) #f rx #f ry
                   :fill-color (clamp r g b a)))))


(defun render-arc (cmd)
 (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                    (r :color :r) (g :color :g) (b :color :b) (a :color :a))
     (cmd %nk:command-arc)
   (draw-arc (vec2 #f x #f y) #f radius #f a0 #f a1
             :stroke-color (clamp r g b a))))


(defun render-arc-filled (cmd)
  (dissect-c-struct ((x :cx) (y :cy) (radius :r) (a0 :a 0) (a1 :a 1)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-arc-filled)
    (draw-arc (vec2 #f x #f y) #f radius #f a0 #f a1 :fill-color (clamp r g b a))))


(defun render-triangle (cmd)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle)
    (draw-polygon (list (vec2 #f x0 #f y0)
                        (vec2 #f x1 #f y1)
                        (vec2 #f x2 #f y2))
                  :stroke-color (clamp r g b a))))


(defun render-triangle-filled (cmd)
  (dissect-c-struct ((x0 :a :x) (y0 :a :y)
                     (x1 :b :x) (y1 :b :y)
                     (x2 :c :x) (y2 :c :y)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-triangle-filled)
    (draw-polygon (list (vec2 #f x0 #f y0)
                        (vec2 #f x1 #f y1)
                        (vec2 #f x2 #f y2))
                  :fill-color (clamp r g b a))))


(defun render-polygon (cmd)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :x)
                                     #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :y)))))
    (draw-polygon vertices :stroke-color (clamp r g b a)))))


(defun render-polygon-filled (cmd)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polygon-filled)
    (let ((vertices (loop for i from 0 below count
                       collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :x)
                                     #f(c-ref cmd (:struct (%nk:command-polygon))
                                              :points i :y)))))
    (draw-polygon vertices :fill-color (clamp r g b a)))))


(defun render-polyline (cmd)
  (dissect-c-struct ((count :point-count)
                     (r :color :r) (g :color :g) (b :color :b) (a :color :a))
      (cmd %nk:command-polyline)
    (let ((points (loop for i from 0 below count
                     collect (vec2 #f(c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :x)
                                   #f(c-ref cmd (:struct (%nk:command-polygon))
                                            :points i :y)))))
      (draw-polyline points (clamp r g b a)))))


(defun render-boxed-text (cmd poiu)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (length :length)
                     (height :height)
                     (string :string &)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (let ((lisp-string (cffi:foreign-string-to-lisp string :count length)))
      (render-text (text-renderer-of poiu) lisp-string
                   :position (vec2 #f x #f y)
                   :color (clamp r g b a)))))


(defun render-text-bounding-box (cmd)
  (dissect-c-struct ((x :x) (y :y) (w :w) (h :h)
                     (r :foreground :r) (g :foreground :g)
                     (b :foreground :b) (a :foreground :a))
      (cmd %nk:command-text)
    (draw-bounding-box x y w h r g b a)))


(defun render-image (cmd)
  (declare (ignore cmd)))


(defun render-poiu (&optional (poiu *context*))
  (let (commands)
    (with-canvas ((canvas-of poiu) (floor (width-of poiu)) (floor (height-of poiu)))
      (bodge-nuklear:docommands (cmd (handle-value-of poiu))
        (case (autowrap:enum-key '(:enum (%nk:command-type))
                                 (c-ref cmd (:struct (%nk:command)) :type))
          (:nop)
          (:scissor (render-scissor cmd))
          (:line (render-line cmd))
          (:curve (render-curve cmd))
          (:rect (render-rect cmd))
          (:rect-filled (render-rect-filled cmd))
          (:rect-multi-color (render-rect-multi-color cmd))
          (:circle (render-circle cmd))
          (:circle-filled (render-circle-filled cmd))
          (:arc (render-arc cmd))
          (:arc-filled (render-arc-filled cmd))
          (:triangle (render-triangle cmd))
          (:triangle-filled (render-triangle-filled cmd))
          (:polygon (render-polygon cmd))
          (:polygon-filled (render-polygon-filled cmd))
          (:polyline (render-polyline cmd))
          (:text (push cmd commands))
          (:image (render-image cmd)))))
    (dolist (cmd commands)
      (render-boxed-text cmd poiu))))
