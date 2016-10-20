#lang racketscript/base

;; Emulates 2htdp/image library as much as possible. Also see
;; Whalesong's implementation, which we have referrred

(require (for-syntax racketscript/base
                     syntax/parse)
         racket/bool
         "private/color.rkt"
         "../private/jscommon.rkt")

(provide empty-image
         empty-scene

         image-height
         image-width

         line
         rectangle
         circle
         text

         place-image
         place-images
         overlay/align
         overlay/xy
         overlay
         underlay
         underlay/xy
         above/align
         above
         beside/align
         beside

         rotate
         scale

         print-image
         color
         (struct-out posn))

;;-----------------------------------------------------------------------------
;; Macros for drawing

(define-struct posn (x y) #:transparent)

(define-syntax-rule (δ-move ctx x y δx δy)
  (#js.ctx.moveTo (+ x δx) (+ y δy)))

(define-syntax-rule (with-context ctx body ...)
  (begin (#js.ctx.save)
         body ...
         (#js.ctx.restore)))

(define-syntax-rule (with-origin ctx [x y] body ...)
  (with-context ctx (#js.ctx.translate x y) body ...))

(define (->web-color p)
  (cond
    [(string? p) (string->web-color p)]
    [(color? p) (color->web-color p)]
    [(symbol? p) (string->web-color (symbol->string p))]
    [else (error 'color "invalid color")]))

(define-syntax (with-path stx)
  (syntax-parse stx
    [(with-path ctx:id {mode pen} body ...)
     #`(begin (#js.ctx.beginPath)
              body ...
              #,(when (syntax-e #'pen)
                  #`(cond
                      [(or (equal? mode 'outline) (equal? mode "outline"))
                       (:= #js.ctx.strokeStyle (->web-color pen))
                       (#js.ctx.stroke)]
                      [(or (equal? mode 'solid) (equal? mode "solid"))
                       (:= #js.ctx.fillStyle (->web-color pen))
                       (#js.ctx.fill)]))
              (#js.ctx.closePath))]))

;;-----------------------------------------------------------------------------
;; Display images on browser

(define (print-image d)
  (define canvas (#js.document.createElement "canvas"))
  (define ctx (#js.canvas.getContext "2d"))

  (#js.document.body.appendChild canvas)
  (#js.document.body.appendChild (#js.document.createElement "br"))

  (:= #js.canvas.width #js.d.width)
  (:= #js.canvas.height #js.d.height)

  (#js.console.log d)

  (with-origin ctx [(half #js.d.width) (half #js.d.height)]
    (#js.d.render ctx 0 0)))

(define *invisible-canvas-context*
  (let ([canvas (#js.document.createElement "canvas")])
    (#js.canvas.getContext "2d")))

;;-----------------------------------------------------------------------------
;; Some common shapes

(define empty-image
  ($/obj [type    "empty-image"]
         [width   0]
         [height  0]
         [render
          (λ (ctx x y) (void))]))

(define (image-height i)  #js.i.height)
(define (image-width i)   #js.i.width)

(define-proto EmptyScene
  #:init
  (λ (width height borders?)
    (set-object! #js*.this
                 [type      "empty-scene"]
                 [width     width]
                 [height    height]
                 [borders?  borders?]))
  #:prototype-fields
  [render (λ (ctx x y)
            ;; TODO: borders?
            (void))])

(define-proto Text
  #:init
  (λ (text size color face family style weight underline?)
    (set-object! #js*.this
                 [type       "text"]
                 [text       text]
                 [size       size]
                 [color      (->web-color color)]
                 [face       face]
                 [family     family]
                 [style      style]
                 [weight     weight]
                 [underline  underline?])
    (#js*.this._updateMetrics))
  #:prototype-fields
  [_updateMetrics
   (λ ()
     (define font (++ #js*.this.weight " "
                      #js*.this.style " "
                      #js*.this.size "px "
                      #js*.this.face " "
                      #js*.this.family))

     (:= ($ *invisible-canvas-context* 'font) font)
     (define metrics ($> *invisible-canvas-context*
                         (measureText #js*.this.text)))

     (set-object! #js*.this
                  [font    font]
                  [width   #js.metrics.width]
                  [height  #js*.this.size]))]
  [render
   (λ (ctx x y)
     (with-origin ctx [x y]
       (set-object! ctx
                    [font          #js*.this.font]
                    [textAlign     "center"]
                    [textBaseline  "middle"]
                    [fillStyle     #js*.this.color])
       (#js.ctx.fillText #js*.this.text 0 0)))])

(define-proto Line
  #:init
  (λ (x y pen-or-color)
    (set-object! #js*.this
                 [type    "line"]
                 [width   (abs+ceil x)]
                 [height  (abs+ceil y)]
                 [mode    #f]
                 [pen     pen-or-color]))
  #:prototype-fields
  [render (λ (ctx x y)
            (with-origin ctx [x y]
              (with-path ctx {"outline" #js*.this.pen}
                (define sx (- (abs (half #js*.this.x))))
                (define sy (- (abs (half #js*.this.y))))
                (cond
                  [(and (>= x 0) (>= y 0))
                   (#js.ctx.moveTo sx sy)
                   (#js.ctx.lineTo (+ sx x) (+ sy y))]
                  [(and (>= x 0) (< y 0))
                   (#js.ctx.moveTo (+ sx x) sy)
                   (#js.ctx.lineTo sx (- sy y))]
                  [(and (< x 0) (>= y 0))
                   (#js.ctx.moveTo sx (+ sy y))
                   (#js.ctx.lineTo (- sx x) sy)]
                  [(and (< x 0) (< y 0))
                   (#js.ctx.moveTo (- sx x) (- sy y))
                   (#js.ctx.lineTo sx sy)]))))])

(define-proto Rectangle
  #:init
  (λ (width height mode pen-or-color)
    (set-object! #js*.this
      [type     "rectangle"]
      [width    width]
      [height   height]
      [mode     mode]
      [pen      pen-or-color]))
  #:prototype-fields
  [render
   (λ (ctx x y)
     (with-origin ctx [x y]
       (with-path ctx {#js*.this.mode #js*.this.pen}
         (let* ([width     #js*.this.width]
                [height    #js*.this.height]
                [start-x   (- (half width))]
                [start-y   (- (half height))])
           (#js.ctx.rect start-x start-y width height)))))])

(define-proto Circle
  #:init
  (λ (radius mode pen-or-color)
    (define diameter (twice radius))
    (set-object! #js*.this
      [type     "circle"]
      [radius   radius]
      [width    diameter]
      [height   diameter]
      [mode     mode]
      [pen      pen-or-color]))
  #:prototype-fields
  [render
   (λ (ctx x y)
     (define radius #js*.this.radius)
     (with-origin ctx [x y]
       (with-path ctx {#js*.this.mode #js*.this.pen}
         (#js.ctx.ellipse 0 0            ;; center
                          radius radius  ;; radius-x, radius-y
                          0 0 (twice #js.Math.PI)))))])

(define (empty-scene width height)
  (new (EmptyScene width height #f)))

(define (text txt size color)
  (new (Text txt
             size
             color
             ""         ;; face
             "serif"    ;; family
             "normal"   ;; style
             "normal"   ;; weight
             #f)))      ;; underline


(define (line x y pen-or-color)
  (new (Line x y pen-or-color)))

(define (rectangle w h m p)
  (new (Rectangle w h m p)))

(define (circle r m p)
  (new (Circle r m p)))


;;-----------------------------------------------------------------------------
;; Combine images

(define-proto Overlay
  #:init
  (λ (x-place y-place ima imb)
    (define ima-cx (half #js.ima.width))
    (define ima-cy (half #js.ima.height))
    (define imb-cx (half #js.imb.width))
    (define imb-cy (half #js.imb.height))

    ;; When aligning to an edge, new height are width are maximum of
    ;; two images, while when we give an xy offset, depnding on size
    ;; of both images, the dimensions of increase or remain same.
    (define width   (case x-place
                      [("beside") (+ #js.ima.width #js.imb.width)]
                      [else       (if (number? x-place)
                                      (if (positive? x-place)
                                          (max #js.ima.width (+ #js.imb.width x-place))
                                          (max #js.imb.width (- #js.ima.width x-place)))
                                      (max #js.ima.width #js.imb.width))]))
    (define height  (case y-place
                      [("above")  (+ #js.ima.height #js.imb.height)]
                      [else       (if (number? y-place)
                                      (if (positive? y-place)
                                          (max #js.ima.height (+ #js.imb.height y-place))
                                          (max #js.imb.height (- #js.ima.height y-place)))
                                      (max #js.ima.height #js.imb.height))]))

    (define δ-edge-x (half width))
    (define δ-edge-y (half height))

    ;; Center of image is (0, 0), which is also center of bigger
    ;; image. Calculate the distance of centers of images from this
    ;; final center.
    ;; 
    ;; When given an xy offset, we start by placing each image on top
    ;; of each other. We line up the images at upper-right corner of
    ;; new canvas to start with and then translate the appropriate
    ;; image depending on whether we are given positive or negative
    ;; offset. To line up at upper right corner of new image, we
    ;; calculate distance between the centers of final and local
    ;; setting and move both images by that amount.
    (define-values (δ-a-x δ-b-x)
      (case x-place
        [("left")             (values (- ima-cx δ-edge-x)
                                      (- imb-cx δ-edge-x))]
        [("right")            (values (- δ-edge-x ima-cx)
                                      (- δ-edge-x imb-cx))]
        [("beside")           (values (- ima-cx δ-edge-x)
                                      (- δ-edge-x imb-cx))]
        [("middle" "center")  (values 0 0)]
        [else
         (cond
           [(number? x-place)
            (define local-width (max #js.ima.width #js.imb.width))
            (define local-acx (- ima-cx (half local-width)))
            (define local-bcx (- imb-cx (half local-width)))
            (define acx-bcx (- δ-edge-x (half local-width)))
            (if (positive? x-place)
                (values (- local-acx acx-bcx) (+ x-place (- local-bcx acx-bcx)))
                (values (- local-acx x-place acx-bcx) (- local-bcx acx-bcx)))]
           [else (error "invalid x-place align")])]))

    (define-values (δ-a-y δ-b-y)
      (case y-place
        [("top")               (values (- ima-cy δ-edge-y)
                                       (- imb-cy δ-edge-y))]
        [("bottom")            (values (- δ-edge-y ima-cy)
                                       (- δ-edge-y imb-cy))]
        [("above")             (values (- ima-cy δ-edge-y)
                                       (- δ-edge-y imb-cy))]
        [("middle" "center")   (values 0 0)]
        [else
         (cond
           [(number? y-place)
            (define local-height (max #js.ima.height #js.imb.height))
            (define local-acy (- ima-cy (half local-height)))
            (define local-bcy (- imb-cy (half local-height)))
            (define acy-bcy (- δ-edge-y (half local-height)))
            (if (positive? y-place)
                (values (- local-acy acy-bcy) (+ y-place (- local-bcy acy-bcy)))
                (values (- local-acy y-place acy-bcy) (- local-bcy acy-bcy)))]
           [else
             (error "invalid y-place align")])]))

    (set-object! #js*.this
      [type       "overlay"]
      [ima        ima]
      [imb        imb]
      [width      width]
      [height     height]
      [aDx        δ-a-x]
      [aDy        δ-a-y]
      [bDx        δ-b-x]
      [bDy        δ-b-y]))
  #:prototype-fields
  [render
   (λ (ctx x y)
     (define ima #js*.this.ima)
     (define imb #js*.this.imb)
     (with-origin ctx [x y]
       (#js.imb.render ctx #js*.this.bDx #js*.this.bDy)
       (#js.ima.render ctx #js*.this.aDx #js*.this.aDy)))])

(define-proto Container
  #:init
  (λ (childs posns width height)
    (set-object! #js*.this
      [type     "container"]
      [childs   childs]
      [posns    posns]
      [width    width]
      [height   height]))
  #:prototype-fields
  [render
   (λ (ctx x y)
     (define width   #js*.this.width)
     (define height  #js*.this.height)

     (with-origin ctx [(- x (half width)) (- y (half height))]
       (#js.ctx.beginPath)
       (#js.ctx.rect 0 0 (sub1 width) (sub1 height))
       (#js.ctx.clip)
       (let loop ([childs #js*.this.childs]
                  [posns  #js*.this.posns])
         (unless (null? childs)
           (define child (car childs))
           (define posn (car posns))
           (#js.child.render ctx (posn-x posn) (posn-y posn))
           (loop (cdr childs) (cdr posns))))))])

;;-----------------------------------------------------------------------------
;; Transform images

;; Rotate clockwise
;; TODO: Rotated bouding box is not actually right.
(define-proto Rotate
  #:init
  (λ (image angle)
    (define width #js.image.width)
    (define height #js.image.height)
    (define θ (/ (* #js.Math.PI angle) 180.0))

    (define sin-θ (sin θ))
    (define cos-θ (cos θ))

    ;; (w, 0) rotation
    (define x1 (* cos-θ width))
    (define y1 (* sin-θ width))

    ;; (0, h) rotation
    (define x2 (* (- sin-θ) height))
    (define y2 (* cos-θ height))

    ;; (w, h) rotation
    (define x3 (+ x1 x2))
    (define y3 (+ y1 y2))

    (define min-x (min 0 x1 x2 x3))
    (define max-x (max 0 x1 x2 x3))
    (define min-y (min 0 y1 y2 y3))
    (define max-y (max 0 y1 y2 y3))

    (define rotated-width   (floor (- max-x min-x)))
    (define rotated-height  (floor (- max-y min-y)))

    (set-object! #js*.this
                 [image        image]
                 [width        rotated-width]
                 [height       rotated-height]
                 [degrees      angle]
                 [radians      θ]))
  #:prototype-fields
  [render
   (λ (ctx x y)1
     (with-origin ctx [x y]
       (#js.ctx.rotate #js*.this.radians)
       (#js*.this.image.render ctx 0 0)))])

(define-proto Scale
  #:init
  (λ (image x-factor y-factor)
    (set-object! #js*.this
                 [image        image]
                 [x-factor     x-factor]
                 [y-factor     y-factor]
                 [width        (floor (* #js.image.width x-factor))]
                 [height       (floor (* #js.image.height y-factor))]))
  #:prototype-fields
  [render
   (λ (ctx x y)
     (with-origin ctx [x y]
       (#js.ctx.scale #js*.this.x-factor #js*.this.y-factor)
       (#js*.this.image.render ctx 0 0)))])

(define (container childs posns width height)
  (new (Container childs posns width height)))

(define (place-image child cx cy base)
  (let ([width  #js.base.width]
        [height #js.base.height])
    (container (list base child)
               (list (posn (half width) (half height))
                     (posn cx cy))
               width
               height)))

(define (place-images images posns scene)
  (let ([width  #js.scene.width]
        [height #js.scene.height])
    (container (cons scene images)
               (cons (posn (half width) (half height)) posns)
               width
               height)))

(define-syntax-rule (-overlay/align combiner x-place y-place ima imb imn)
  ;; Exists because using apply everytime is slower, so we just
  ;; inline it
  (foldl (λ (img acc)
           (combiner x-place y-place acc img))
         empty-image
         (cons ima (cons imb imn))))

(define (-single-overlay x-place y-place ima imb)
  (new (Overlay x-place y-place ima imb)))

(define (-single-underlay x-place y-place ima imb)
  (new (Overlay x-place y-place imb ima)))

(define (overlay/align x-place y-place ima imb . imn)
  (-overlay/align -single-overlay x-place y-place ima imb imn))

(define (overlay ima imb . imn)
  (-overlay/align -single-overlay "middle" "middle" ima imb imn))

(define (overlay/xy ima x y imb)
  (new (Overlay x y ima imb)))

(define (underlay ima imb . imn)
  (-overlay/align -single-underlay "middle" "middle" ima imb imn))

(define (underlay/align x-place y-place ima imb . imn)
  (-overlay/align -single-underlay x-place y-place ima imb imn))

(define (underlay/xy ima x y imb)
  (new (Overlay (- x) (- y) imb ima)))

(define (above/align x-place i1 i2 . is)
  (-overlay/align -single-overlay x-place "above" i1 i2 is))

(define (above i1 i2 . is)
  (-overlay/align -single-overlay "middle" "above" i1 i2 is))

(define (beside/align y-place i1 i2 . is)
  (-overlay/align -single-overlay "beside" y-place i1 i2 is))

(define (beside i1 i2 . is)
  (-overlay/align -single-overlay "beside" "middle" i1 i2 is))

(define (rotate angle image)
  ;; Rotate counter-clockwise
  (new (Rotate image (- angle))))

(define (scale factor image)
  (new (Scale image factor factor)))
