(cl:in-package :cl-bodge.ui)


(defun render-scissor ()
  (scissors (bodge-ui:scissor-origin) (bodge-ui:scissor-width) (bodge-ui:scissor-height)))


(defun render-line ()
  (draw-line (bodge-ui:line-origin)
             (bodge-ui:line-end)
             (bodge-ui:line-color)
             :thickness (bodge-ui:line-thickness)))


(defun render-curve ()
  (draw-curve (bodge-ui:curve-origin)
              (bodge-ui:curve-end)
              (bodge-ui:curve-first-control-point)
              (bodge-ui:curve-second-control-point)
              (bodge-ui:curve-color)
              :thickness (bodge-ui:curve-thickness)))


(defun render-rect ()
  (draw-rect (bodge-ui:rect-origin)
             (bodge-ui:rect-width) (bodge-ui:rect-height)
             :stroke-paint (bodge-ui:rect-stroke-color)
             :rounding (bodge-ui:rect-rounding)
             :thickness (bodge-ui:rect-stroke-thickness)))


(defun render-rect-filled ()
  (draw-rect (bodge-ui:filled-rect-origin)
             (bodge-ui:filled-rect-width) (bodge-ui:filled-rect-height)
             :fill-paint (bodge-ui:filled-rect-color)
             :rounding (bodge-ui:filled-rect-rounding)))


(defun render-rect-multi-color ())


(defun render-circle ()
  (draw-ellipse (bodge-ui:ellipse-origin)
                (bodge-ui:ellipse-radius-x) (bodge-ui:ellipse-radius-y)
                :stroke-paint (bodge-ui:ellipse-stroke-color)
                :thickness (bodge-ui:ellipse-stroke-thickness)))


(defun render-circle-filled ()
  (draw-ellipse (bodge-ui:filled-ellipse-origin)
                (bodge-ui:filled-ellipse-radius-x) (bodge-ui:filled-ellipse-radius-y)
                :fill-paint (bodge-ui:filled-ellipse-color)))


(defun render-arc ()
  (draw-arc (bodge-ui:arc-origin)
            (bodge-ui:arc-radius)
            (bodge-ui:arc-start-angle) (bodge-ui:arc-end-angle)
            :stroke-paint (bodge-ui:arc-stroke-color)
            :thickness (bodge-ui:arc-stroke-thickness)))


(defun render-arc-filled ()
  (draw-arc (bodge-ui:filled-arc-origin)
            (bodge-ui:filled-arc-radius)
            (bodge-ui:filled-arc-start-angle) (bodge-ui:filled-arc-end-angle)
            :fill-paint (bodge-ui:filled-arc-color)))


(defun render-triangle ()
  (draw-polygon (list (bodge-ui:triangle-origin)
                      (bodge-ui:triangle-second-vertex)
                      (bodge-ui:triangle-third-vertex))
                :stroke-paint (bodge-ui:triangle-stroke-color)
                :thickness (bodge-ui:triangle-stroke-thickness)))


(defun render-triangle-filled ()
  (draw-polygon (list (bodge-ui:filled-triangle-origin)
                      (bodge-ui:filled-triangle-second-vertex)
                      (bodge-ui:filled-triangle-third-vertex))
                :fill-paint (bodge-ui:filled-triangle-color)))


(defun render-polygon ()
  (draw-polygon (bodge-ui:polygon-vertices)
                :stroke-paint (bodge-ui:polygon-stroke-color)
                :thickness (bodge-ui:polygon-stroke-thickness)))


(defun render-polygon-filled ()
  (draw-polygon (bodge-ui:filled-polygon-vertices)
                :fill-paint (bodge-ui:filled-polygon-color)))


(defun render-polyline ()
  (draw-polyline (bodge-ui:polyline-vertices)
                 (bodge-ui:polyline-color)
                 :thickness (bodge-ui:polyline-thickness)))


(defun render-text ()
  (let ((origin (bodge-ui:text-box-origin)))
    (draw-text (vec2 (x origin) (- (y origin) (canvas-font-descender (canvas-font-metrics))))
               (bodge-ui:text-string)
               :fill-color (bodge-ui:text-foreground-color))))


(defun render-image ())


(defcanvas ui-canvas ()
  (bodge-ui:docommands ()
    (case (bodge-ui:command-type)
      (:nop)
      (:scissor (render-scissor))
      (:line (render-line))
      (:curve (render-curve))
      (:rect (render-rect))
      (:rect-filled (render-rect-filled))
      (:rect-multi-color (render-rect-multi-color))
      (:circle (render-circle))
      (:circle-filled (render-circle-filled))
      (:arc (render-arc))
      (:arc-filled (render-arc-filled))
      (:triangle (render-triangle))
      (:triangle-filled (render-triangle-filled))
      (:polygon (render-polygon))
      (:polygon-filled (render-polygon-filled))
      (:polyline (render-polyline))
      (:text (render-text))
      (:image (render-image)))))


(defclass ui-font (bodge-ui:custom-font)
  ((canvas :initarg :canvas)))


(defmethod bodge-ui:calculate-text-width ((font ui-font) string)
  (with-slots (canvas) font
    (canvas-text-advance string canvas)))


(defmethod bodge-ui:text-line-height ((font ui-font))
  (with-slots (canvas) font
    (canvas-font-line-height (canvas-font-metrics canvas))))


(defclass ui-renderer (disposable)
  ((font :reader bodge-ui:renderer-default-font)
   (canvas :reader %canvas-of)))


(defmethod initialize-instance :after ((this ui-renderer) &key width height
                                                            antialiased
                                                            pixel-ratio)
  (with-slots (font canvas) this
    (setf canvas (make-canvas 'ui-canvas width height
                              :antialiased antialiased
                              :pixel-ratio pixel-ratio)
          font (make-instance 'ui-font :canvas canvas))))


(define-destructor ui-renderer (font canvas)
  (dispose canvas)
  (dispose font))


(define-system-function make-ui-canvas-renderer graphics-system
    (width height &key (antialiased t) (pixel-ratio 1f0))
  (make-instance 'ui-renderer :width width :height height
                              :antialiased antialiased :pixel-ratio pixel-ratio))


(defmethod bodge-ui:renderer-canvas-width ((this ui-renderer))
  (with-slots (canvas) this
    (width-of canvas)))


(defmethod bodge-ui:renderer-canvas-height ((this ui-renderer))
  (with-slots (canvas) this
    (height-of canvas)))


(defmethod bodge-ui:render-ui ((this ui-renderer))
  (render t (%canvas-of this)))


(defun update-renderer-canvas-size (renderer width height)
  (with-slots (canvas scale) renderer
    (update-canvas-size canvas width height)))


(defun update-renderer-canvas-pixel-ratio (ui pixel-ratio)
  (with-slots (canvas) ui
    (update-canvas-pixel-ratio canvas pixel-ratio)))
