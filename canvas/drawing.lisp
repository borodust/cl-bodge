(cl:in-package :cl-bodge.canvas)


(defun draw-line (origin end paint &key (thickness 1.0))
  (bodge-canvas:draw-line origin end (%paint-handle-of paint)
                          :thickness thickness))


(defun draw-curve (origin end ctrl0 ctrl1 paint &key (thickness 1.0))
  (bodge-canvas:draw-curve origin end ctrl0 ctrl1 (%paint-handle-of paint)
                           :thickness thickness))


(defun draw-rect (origin w h &key (fill-paint nil)
                               (stroke-paint nil)
                               (thickness 1.0)
                               (rounding 0.0))
  (unless (or (zerop w) (zerop h))
    (bodge-canvas:draw-rect origin w h
                            :fill-paint (%paint-handle-of fill-paint)
                            :stroke-paint (%paint-handle-of stroke-paint)
                            :thickness thickness
                            :rounding rounding)))


(defun draw-image (origin w h image-paint &key (stroke-paint nil)
                                            (thickness 1.0)
                                            (rounding 0.0)
                                            (scale-x 1.0)
                                            (scale-y 1.0)
                                            (translate-x 0.0)
                                            (translate-y 0.0)
                                            (rotate 0.0))
  (unless (or (zerop w) (zerop h))
    (bodge-canvas:draw-image origin w h (%paint-handle-of image-paint)
                             :stroke-paint (%paint-handle-of stroke-paint)
                             :thickness thickness
                             :rounding rounding
                             :scale-x scale-x
                             :scale-y scale-y
                             :translate-x translate-x
                             :translate-y translate-y
                             :rotate rotate)))


(defun draw-circle (center radius &key (fill-paint nil)
                                    (stroke-paint nil)
                                    (thickness 1.0))
  (bodge-canvas:draw-circle center radius
                            :fill-paint (%paint-handle-of fill-paint)
                            :stroke-paint (%paint-handle-of stroke-paint)
                            :thickness thickness))


(defun draw-ellipse (center x-radius y-radius &key (fill-paint nil)
                                                (stroke-paint nil)
                                                (thickness 1.0))
  (unless (or (zerop x-radius) (zerop y-radius))
    (bodge-canvas:draw-ellipse center x-radius y-radius
                               :fill-paint (%paint-handle-of fill-paint)
                               :stroke-paint (%paint-handle-of stroke-paint)
                               :thickness thickness)))


(defun draw-arc (center radius a0 a1 &key (fill-paint nil)
                                       (stroke-paint nil)
                                       (thickness 1.0))
  (bodge-canvas:draw-arc center radius a0 a1
                         :fill-paint (%paint-handle-of fill-paint)
                         :stroke-paint (%paint-handle-of stroke-paint)
                         :thickness thickness))


(defun draw-polygon (vertices &key (fill-paint nil)
                                (stroke-paint nil)
                                (thickness 1.0))
  (bodge-canvas:draw-polygon vertices
                             :fill-paint (%paint-handle-of fill-paint)
                             :stroke-paint (%paint-handle-of stroke-paint)
                             :thickness thickness))


(defun draw-polyline (points paint &key (thickness 1.0))
  (bodge-canvas:draw-polyline points (%paint-handle-of paint)
                              :thickness thickness))


(defun draw-text (position text &optional (paint *black*))
  (bodge-canvas:draw-text position text (%paint-handle-of paint)))
