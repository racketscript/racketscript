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
         square
         circle
         text
         text/font
         triangle
         frame
         color-frame

         place-image
         place-images
         place-image/align
         place-images/align
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
         flip-vertical
         flip-horizontal

         bitmap/data
         bitmap/url
         freeze

         print-image
         color
         (rename-out [color make-color])
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
                       (:= #js.ctx.strokeStyle (js-string (->web-color pen)))
                       (#js.ctx.stroke)]
                      [(or (equal? mode 'solid) (equal? mode "solid"))
                       (:= #js.ctx.fillStyle (js-string (->web-color pen)))
                       (#js.ctx.fill)]))
              (#js.ctx.closePath))]))

;;-----------------------------------------------------------------------------
;; Display images on browser

(define (print-image d)
  (define canvas (#js.document.createElement #js"canvas"))
  (define ctx (#js.canvas.getContext #js"2d"))

  (#js.document.body.appendChild canvas)
  (#js.document.body.appendChild (#js.document.createElement #js"br"))

  (:= #js.canvas.width #js.d.width)
  (:= #js.canvas.height #js.d.height)

  (with-origin ctx [(half #js.d.width) (half #js.d.height)]
    (#js.d.render ctx 0 0)))

(define *invisible-canvas-context*
  (let ([canvas (#js.document.createElement #js"canvas")])
    (#js.canvas.getContext #js"2d")))

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

(define-proto Text
  (λ (text size color face family style weight underline?)
    #:with-this this
    (set-object! this
                 [type       "text"]
                 [text       text]
                 [size       size]
                 [color      (->web-color color)]
                 [face       face]
                 [family     family]
                 [style      style]
                 [weight     weight]
                 [underline  underline?])
    (#js.this._updateMetrics))
  [_updateMetrics
   (λ ()
     #:with-this this
     (define font (string-append
                     #js.this.weight " "
                     #js.this.style " "
                     (number->string #js.this.size) "px "
                     #js.this.face " "
                     #js.this.family))

     (:= ($ *invisible-canvas-context* 'font) (js-string font))
     (define metrics ($> *invisible-canvas-context*
                         (measureText (js-string #js.this.text))))

     (set-object! this
                  [font    font]
                  [width   #js.metrics.width]
                  [height  #js.this.size]))]
  [render
   (λ (ctx x y)
     #:with-this this
     (with-origin ctx [x y]
       (set-object! ctx
                    [font          (js-string #js.this.font)]
                    [textAlign     #js"center"]
                    [textBaseline  #js"middle"]
                    [fillStyle     (js-string #js.this.color)])
       (#js.ctx.fillText (js-string #js.this.text) 0 0)))])

(define-proto Line
  (λ (x y pen-or-color)
    #:with-this this
    (set-object! this
                 [type    "line"]
                 [width   (abs+ceil x)]
                 [height  (abs+ceil y)]
                 [mode    #f]
                 [pen     pen-or-color]))
  [render (λ (ctx x y)
            #:with-this this
            (with-origin ctx [x y]
              (with-path ctx {"outline" #js.this.pen}
                (define sx (- (abs (half #js.this.x))))
                (define sy (- (abs (half #js.this.y))))
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
  (λ (width height mode pen-or-color)
    #:with-this this
    (set-object! this
      [type     "rectangle"]
      [width    width]
      [height   height]
      [mode     mode]
      [pen      pen-or-color]))
  [render
   (λ (ctx x y)
     #:with-this this
     (with-origin ctx [x y]
       (with-path ctx {#js.this.mode #js.this.pen}
         (let* ([width     #js.this.width]
                [height    #js.this.height]
                [start-x   (- (half width))]
                [start-y   (- (half height))])
           (#js.ctx.rect start-x start-y width height)))))])

(define-proto Circle
  (λ (radius mode pen-or-color)
    #:with-this this
    (define diameter (twice radius))
    (set-object! this
      [type     "circle"]
      [radius   radius]
      [width    diameter]
      [height   diameter]
      [mode     mode]
      [pen      pen-or-color]))
  [render
   (λ (ctx x y)
     #:with-this this
     (define radius #js.this.radius)
     (with-origin ctx [x y]
       (with-path ctx {#js.this.mode #js.this.pen}
         (#js.ctx.ellipse 0 0            ;; center
                          radius radius  ;; radius-x, radius-y
                          0 0 (twice #js.Math.PI)))))])

(define-proto Polygon
  (λ (vertices mode pen-or-color)
    #:with-this this
    (define xs (map posn-x vertices))
    (define ys (map posn-y vertices))

    (define width (- (apply max xs) (apply min xs)))
    (define height (- (apply max ys) (apply min xs)))

    (set-object! this
                 [type       "polygon"]
                 [vertices   vertices]
                 [width      width]
                 [height     height]
                 [mode       mode]
                 [pen        pen-or-color]))
  [render
   (λ (ctx x y)
     #:with-this this
     (define first-point (car #js.this.vertices))
     (define rest-points (cdr #js.this.vertices))
     (define radius #js.this.radius)
     (with-origin ctx [x y]
       (with-path ctx {#js.this.mode #js.this.pen}
         (#js.ctx.moveTo (posn-x first-point) (posn-y first-point))
         (let loop ([points rest-points])
           (unless (null? points)
             (define pt (car points))
             (#js.ctx.lineTo (posn-x pt) (posn-y pt))
             (loop (cdr points)))))))])

(define (empty-scene width height)
  (rectangle width height "solid" "white")
  (rectangle width height "outline" "black"))   

(define (text txt size color)
  (new (Text txt
             size
             color
             ""         ;; face
             "serif"    ;; family
             "normal"   ;; style
             "normal"   ;; weight
             #f)))      ;; underline

(define (text/font txt size color face family style weight underline?)
  (new (Text txt
             size
             color
             (or face "")
             (if (symbol? family) (symbol->string family) family)
             (if (symbol? style) (symbol->string style) style)
             (if (symbol? weight) (symbol->string weight) weight)
              underline?)))

(define (line x y pen-or-color)
  (new (Line x y pen-or-color)))

(define (rectangle w h m p)
  (new (Rectangle w h m p)))

(define (square s m p)
  (new (Rectangle s s m p)))

(define (circle r m p)
  (new (Circle r m p)))

(define (triangle side mode color)
  ;; height = side * sin(45)
  (define height (* side (/ (sqrt 3) 2)))
  (new (Polygon (list (posn (- (/ side 2)) (/ height 2))
                      (posn 0 (- (/ height 2)))
                      (posn (/ side 2) (/ height 2)))
                mode
                color)))

;;-----------------------------------------------------------------------------
;; Combine images

(define-proto Overlay
  (λ (x-place y-place ima imb)
    #:with-this this
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

    (set-object! this
      [type       "overlay"]
      [ima        ima]
      [imb        imb]
      [width      width]
      [height     height]
      [aDx        δ-a-x]
      [aDy        δ-a-y]
      [bDx        δ-b-x]
      [bDy        δ-b-y]))
  [render
   (λ (ctx x y)
     #:with-this this
     (define ima #js.this.ima)
     (define imb #js.this.imb)
     (with-origin ctx [x y]
       (#js.imb.render ctx #js.this.bDx #js.this.bDy)
       (#js.ima.render ctx #js.this.aDx #js.this.aDy)))])

(define-proto Container
  (λ (childs posns width height)
    #:with-this this
    (set-object! this
      [type     "container"]
      [childs   childs]
      [posns    posns]
      [width    width]
      [height   height]))
  [render
   (λ (ctx x y)
     #:with-this this
     (define width   #js.this.width)
     (define height  #js.this.height)

     (with-origin ctx [(- x (half width)) (- y (half height))]
       (#js.ctx.beginPath)
       (#js.ctx.rect 0 0 (sub1 width) (sub1 height))
       (#js.ctx.clip)
       (let loop ([childs #js.this.childs]
                  [posns  #js.this.posns])
         (unless (null? childs)
           (define child (car childs))
           (define posn (car posns))
           (#js.child.render ctx (posn-x posn) (posn-y posn))
           (loop (cdr childs) (cdr posns))))))])

;;-----------------------------------------------------------------------------
;; Bitmap images

(define-proto Bitmap
  (λ (data)
    #:with-this this
    (define image (new #js*.Image))
    (:= #js.image.crossOrigin #js"anonymous")
    (:= #js.image.src (js-string data))
    (set-object! this
                 [image  image]
                 [width  #js.image.width]
                 [height #js.image.height]))
    [render
     (λ (ctx x y)
       #:with-this this
       (define image #js.this.image)
       (with-origin ctx [x y]
         (#js.ctx.drawImage image
                            (- (half #js.image.width))
                            (- (half #js.image.height)))))])


(define-proto Freeze
  (λ (img)
    #:with-this this
    (define canvas (#js.document.createElement #js"canvas"))
    (define ctx (#js.canvas.getContext #js"2d"))

    (define width #js.img.width)
    (define height #js.img.height)

    (:= #js.canvas.width   width)
    (:= #js.canvas.height  height)
    (with-origin ctx [(half width) (half height)]
      (#js.img.render ctx 0 0))

    (set-object! this
                 [width    width]
                 [height   height]
                 [canvas   canvas]))
  [render
   (λ (ctx x y)
     #:with-this this
     (define width #js.this.width)
     (define height #js.this.height)
     (with-origin ctx [x y]
       (#js.ctx.drawImage #js.this.canvas
                          (- (half width))
                          (- (half height)))))])

;;-----------------------------------------------------------------------------
;; Transform images

;; Rotate clockwise
;; TODO: Rotated bouding box is not actually right.
(define-proto Rotate
  (λ (image angle)
    #:with-this this
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

    (set-object! this
                 [image        image]
                 [width        rotated-width]
                 [height       rotated-height]
                 [degrees      angle]
                 [radians      θ]))
  [render
   (λ (ctx x y)
     #:with-this this
     (with-origin ctx [x y]
       (#js.ctx.rotate #js.this.radians)
       (#js.this.image.render ctx 0 0)))])

(define-proto Scale
  (λ (image x-factor y-factor)
    #:with-this this
    (set-object! this
                 [image        image]
                 [x-factor     x-factor]
                 [y-factor     y-factor]
                 [width        (abs (floor (* #js.image.width x-factor)))]
                 [height       (abs (floor (* #js.image.height y-factor)))]))
  [render
   (λ (ctx x y)
     #:with-this this
     (with-origin ctx [x y]
       (#js.ctx.scale #js.this.x-factor #js.this.y-factor)
       (#js.this.image.render ctx 0 0)))])

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

(define (-align-image-pos image pos x-place y-place)
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define new-x
    (case x-place
      [("left") (+ (half #js.image.width) x)]
      [("right") (- x (half #js.image.width))]
      [("center" "middle") x]))
  (define new-y
    (case y-place
      [("top") (+ (half #js.image.height) y)]
      [("bottom") (- y (half #js.image.height))]
      [("center" "middle") y]))
  (posn new-x new-y))

(define (place-image/align image x y x-place y-place scene)
  (define new-pos (-align-image-pos image (posn x y) x-place y-place))
  (place-image image (posn-x new-pos) (posn-y new-pos) scene))

(define (place-images/align images posns x-place y-place scene)
  (define new-posns (map (λ (i p)
                           (-align-image-pos i p x-place y-place))
                         images
                         posns))
  (place-images images new-posns scene))

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

(define (flip-horizontal image)
  (new (Scale image -1 1)))

(define (flip-vertical image)
  (new (Scale image 1 -1)))

(define (bitmap/data data)
  (new (Bitmap data)))

(define (bitmap/url url)
  (new (Bitmap url)))

(define (frame img)
  (color-frame "black" img))

(define (color-frame color img)
  (overlay (rectangle (image-width img) (image-height img) 'outline color)
           img))

(define (freeze img)
  (new (Freeze img)))
