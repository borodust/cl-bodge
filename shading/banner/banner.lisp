(cl:in-package :cl-bodge.shading)


(defshader (2d-banner-vertex
            (:sources "banner.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-position :location 0)
  (banner-tex-coord :location 1)
  (banner-mvp :name "MVP"))


(defshader (2d-banner-fragment
            (:sources "banner.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-texture :name "banner"))


(defpipeline 2d-banner-pipeline
  :vertex 2d-banner-vertex
  :fragment 2d-banner-fragment)


(defshader (cubemap-banner-vertex
            (:sources "banner_cube.v.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-position :location 0)
  (banner-tex-coord :location 1)
  (banner-mvp :name "MVP"))


(defshader (cubemap-banner-fragment
            (:sources "banner_cube.f.glsl")
            (:base-path :system-relative :cl-bodge/shading "banner/"))
  (banner-texture :name "banner"))


(defpipeline cubemap-banner-pipeline
  :vertex cubemap-banner-vertex
  :fragment cubemap-banner-fragment)


;;;
;;; BANNER
;;;
(defclass banner (disposable)
  ((banner-pipeline :initarg :pipeline :initform (error ":pipeline missing"))
   (position-array :initarg :position-array :initform (error ":position-array missing"))
   (tex-coord-array :initarg :tex-coord-array :initform (error ":tex-coord-array missing"))
   (banner-mvp :initarg :mvp :initform (error ":mvp missing"))))


(define-destructor banner (banner-pipeline position-array tex-coord-array)
  (dispose banner-pipeline)
  (dispose position-array)
  (dispose tex-coord-array))


(defgeneric render-banner (banner texture &key mvp output))

;;;
;;; 2D BANNER
;;;
(defclass 2d-banner (banner) ())


(define-system-function make-2d-banner graphics-system (x y w h &optional (projection (identity-mat4)))
  (let ((pos (make-array-buffer #2a((1 0)
                                    (1 1)
                                    (0 0)
                                    (0 1))))
        (tex (make-array-buffer #2a((1 0)
                                    (1 1)
                                    (0 0)
                                    (0 1)))))
    (make-instance '2d-banner :position-array pos
                              :tex-coord-array tex
                              :pipeline (make-shader-pipeline '2d-banner-pipeline)
                              :mvp (mult projection
                                         (translation-mat4 x y 0)
                                         (scaling-mat4 w h 1)))))


(defmethod render-banner ((this 2d-banner) texture &key mvp (output t))
  (with-slots (banner-pipeline position-array tex-coord-array banner-mvp) this
    (render output banner-pipeline
            :vertex-count 4
            :primitive :triangle-strip
            'banner-position position-array
            'banner-tex-coord tex-coord-array
            'banner-mvp (or mvp banner-mvp)
            'banner-texture texture)))


;;;
;;; CUBEMAP BANNER
;;;
(defclass cubemap-banner (banner)
  ((index-array :initarg :index-array)))


(define-destructor cubemap-banner (index-array)
  (dispose index-array))


;;
;;        10________11
;;         |        |
;;         | pos y  |
;;         |        |
;; 9_______7________5________3________1
;; |       |        |        |        |
;; | neg x | neg z  |  pos x |  pos z |
;; |       |        |        |        |
;; 8_______6________4________2________0
;;         |        |
;;         |        |
;;         | neg y  |
;;        13________12
;;
(define-system-function make-cubemap-banner graphics-system
    (x y w &optional (projection (identity-mat4)))
  (let ((pos (make-array-buffer #2a((3f0 0f0)
                                    (3f0 1f0)
                                    (2f0 0f0)
                                    (2f0 1f0)
                                    (1f0 0f0)
                                    (1f0 1f0)
                                    (0f0 0f0)
                                    (0f0 1f0)
                                    (-1f0 0f0)
                                    (-1f0 1f0)
                                    (0f0 2f0)
                                    (1f0 2f0)
                                    (1f0 -1f0)
                                    (0f0 -1f0))))
        (idx (make-index-buffer #(0 1 2
                                  2 1 3
                                  2 3 4
                                  4 3 5
                                  4 5 6
                                  6 5 7
                                  6 7 8
                                  8 7 9
                                  5 11 7
                                  7 11 10
                                  12 4 13
                                  13 4 6)))
        (tex (make-array-buffer #2a((-0.57735026 -0.57735026 0.57735026)     ; 0
                                    (-0.57735026 0.57735026 0.57735026)      ; 1
                                    (0.57735026 -0.57735026 0.57735026)      ; 2
                                    (0.57735026 0.57735026 0.57735026)       ; 3
                                    (0.57735026 -0.57735026 -0.57735026)     ; 4
                                    (0.57735026 0.57735026 -0.57735026)      ; 5
                                    (-0.57735026 -0.57735026 -0.57735026)    ; 6
                                    (-0.57735026 0.57735026 -0.57735026)     ; 7
                                    (-0.57735026 -0.57735026 0.57735026)     ; 8
                                    (-0.57735026 0.57735026 0.57735026)      ; 9
                                    (-0.57735026 0.57735026 0.57735026)      ; 10
                                    (0.57735026 0.57735026 0.57735026)       ; 11
                                    (0.57735026 -0.57735026 0.57735026)      ; 12
                                    (-0.57735026 -0.57735026 0.57735026))))) ; 13
    (make-instance 'cubemap-banner :pipeline (make-shader-pipeline 'cubemap-banner-pipeline)
                                   :position-array pos
                                   :index-array idx
                                   :tex-coord-array tex
                                   :mvp (mult projection
                                              (translation-mat4 x y 0)
                                              (scaling-mat4 w w 1)))))


(defmethod render-banner ((this cubemap-banner) texture &key mvp (output t))
  (with-slots (banner-pipeline position-array tex-coord-array banner-mvp index-array) this
    (render output banner-pipeline
            :index-buffer index-array
            'banner-position position-array
            'banner-tex-coord tex-coord-array
            'banner-mvp (or mvp banner-mvp)
            'banner-texture texture)))
