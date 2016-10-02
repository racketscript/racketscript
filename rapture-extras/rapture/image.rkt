#lang rapture/base

;; Emulates 2htdp/image library as much as possible. Also see
;; Whalesong's implementation, which we have referrred

(require (for-syntax rapture/base
                     syntax/parse)
         "jscommon.rkt")

(provide empty-image
         empty-scene

         line
         rectangle
         circle

         place-image
         overlay/align
         overlay
         above/align
         above
         beside/align
         beside

         rotate)

;;-----------------------------------------------------------------------------
;; Macros for drawing

(define-syntax-rule (posn xpos ypos)
  ($/obj [x xpos] [y ypos]))

(define-syntax-rule (δ-move ctx x y δx δy)
  (#js.ctx.moveTo (+ x δx) (+ y δy)))

(define-syntax-rule (with-context ctx body ...)
  (begin (#js.ctx.save)
         body ...
         (#js.ctx.restore)))

(define-syntax-rule (with-origin ctx [x y] body ...)
  (with-context ctx (#js.ctx.translate x y) body ...))

(define-syntax (with-path stx)
  (syntax-parse stx
    [(with-path ctx:id {mode pen} body ...)
     #`(begin (#js.ctx.beginPath)
              body ...
              #,(when (syntax-e #'pen)
                  #`(cond
                      [(string=? mode "outline")
                       (:= #js.ctx.strokeStyle (find-color pen))
                       (#js.ctx.stroke)]
                      [(string=? mode "solid")
                       (:= #js.ctx.fillStyle (find-color pen))
                       (#js.ctx.fill)]))
              (#js.ctx.closePath))]))

;;-----------------------------------------------------------------------------
;; Colors and pen

(define (find-color color-string)
  (case color-string
    [("red")    "#ff0000"]
    [("green")  "#00ff00"]
    [("blue")   "#0000ff"]))

;;-----------------------------------------------------------------------------
;; Display images on browser

(define (print-image d)
  (define canvas (#js.document.createElement "canvas"))
  (define ctx (#js.canvas.getContext "2d"))

  (#js.document.body.appendChild canvas)
  (#js.document.body.appendChild (#js.document.createElement "br"))

  (:= #js.canvas.width #js.d.width)
  (:= #js.canvas.height #js.d.height)

  (with-origin ctx [(half #js.d.width) (half #js.d.height)]
    (#js.d.render ctx 0 0)))

;;-----------------------------------------------------------------------------
;; Some common shapes

(define empty-image
  ($/obj [type    "empty-image"]
         [width   0]
         [height  0]
         [render
          (λ (ctx x y) (void))]))

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

    (define width   (case x-place
                      [("beside") (+ #js.ima.width #js.imb.width)]
                      [else       (max #js.ima.width #js.imb.width)]))
    (define height  (case y-place
                      [("above")  (+ #js.ima.height #js.imb.width)]
                      [else       (max #js.ima.height #js.imb.height)]))

    (define δ-edge-x (half width))
    (define δ-edge-y (half height))

    ;; Center of image is (0, 0), which is also center of bigger
    ;; image. Calculate the distance of centers of images from this
    ;; final center.
    (define-values (δ-a-x δ-b-x)
      (case x-place
        [("left")             (values (- ima-cx δ-edge-x)
                                      (- imb-cx δ-edge-x))]
        [("right")            (values (- δ-edge-x ima-cx)
                                      (- δ-edge-x imb-cx))]
        [("beside")           (values (- ima-cx δ-edge-x)
                                      (- δ-edge-x imb-cx))]
        [("middle" "center")  (values 0 0)]))

    (define-values (δ-a-y δ-b-y)
      (case y-place
        [("top")               (values (- ima-cy δ-edge-y)
                                       (- imb-cy δ-edge-y))]
        [("bottom")            (values (- δ-edge-y ima-cy)
                                       (- δ-edge-y imb-cy))]
        [("above")             (values (- ima-cy δ-edge-y)
                                       (- δ-edge-y imb-cy))]
        [("middle" "center")   (values 0 0)]))

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
           (#js.child.render ctx #js.posn.x #js.posn.y)
           (loop (cdr childs) (cdr posns))))))])

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

(define (container childs posns width height)
  (new (Container childs posns width height)))

(define (place-image child cx cy base)
  (let ([width  #js.base.width]
        [height #js.base.height])
    (container (list base child)
               (list (posn (half width) (half height))
                     (posn cx cy))
               #js.base.width
               #js.base.height)))

(define-syntax-rule (-overlay/align x-place y-place ima imb imn)
  ;; Exists because using apply everytime is slower, so we just
  ;; inline it
  (foldl (λ (img acc)
           (new (Overlay x-place y-place acc img)))
         empty-image
         (cons ima (cons imb imn))))

(define (overlay/align x-place y-place ima imb . imn)
  (-overlay/align x-place y-place ima imb imn))

(define (overlay ima imb . imn)
  (-overlay/align "middle" "middle" ima imb imn))

(define (above/align x-place i1 i2 . is)
  (-overlay/align x-place "above" i1 i2 is))

(define (above i1 i2 . is)
  (-overlay/align "middle" "above" i1 i2 is))

(define (beside/align y-place i1 i2 . is)
  (-overlay/align "beside" y-place i1 i2 is))

(define (beside i1 i2 . is)
  (-overlay/align "beside" "middle" i1 i2 is))

(define (rotate angle image)
  ;; Rotate counter-clockwise
  (new (Rotate image (- angle))))
