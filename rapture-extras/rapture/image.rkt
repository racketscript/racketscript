#lang rapture/base

(require (for-syntax rapture/base
                     syntax/parse))

;;-----------------------------------------------------------------------------
;; Interop helpers

(define-syntax  :=        (make-rename-transformer #'$/:=))
(define-syntax  new       (make-rename-transformer #'$/new))

(begin-for-syntax
  (define-syntax-class field
    #:description "a key-value pair for object"
    (pattern [name:id val:expr])))

(define-syntax (define-proto stx)
  (syntax-parse stx
    [(define-proto name:id (~datum #:init) init:expr
       (~datum #:prototype-fields) field:field ...)
     (syntax
      (begin
        (define name init)
        ($/:= ($ name 'prototype 'field.name) field.val) ...))]))

(define-syntax (set-object! stx)
  (syntax-parse stx
    [(set-object! obj:expr f:field ...)
     #`(begin ($ obj 'f.name <:=> f.val) ...)]))

;;-----------------------------------------------------------------------------
;; Helper functions

(define ++        string-append)
(define document  #js*.window.document)
(define Path2D    #js*.window.Path2D)
(define abs       #js*.Math.abs)
(define abs+ceil  (λ (n) (#js*.Math.abs (#js*.Math.ceil n))))
(define (max a b) (if (> a b) a b))

;;-----------------------------------------------------------------------------
;; Macros for drawing

(define-syntax-rule (posn xpos ypos)
  ($/obj [x xpos] [y ypos]))

(define-syntax-rule (twice e)
  (* e 2))

(define-syntax-rule (half e)
  (/ e 2))

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
                       (:= #js.ctx.strokeStyle pen)
                       (#js.ctx.stroke)]
                      [(string=? mode "solid")
                       (:= #js.ctx.fillStyle pen)
                       (#js.ctx.fill)]))
              (#js.ctx.closePath))]))

;;-----------------------------------------------------------------------------
;; Colors and pen

(define (find-color color-string)
  (case color-string
    [("red")    "#ff0000"]
    [("green")  "#00ff00"]
    [("blue")   "#0000ff"]))

(define (string->color/opaque color-str)
  (find-color color-str))

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

(define (line x y pen-or-color)
  (new (Line x y pen-or-color)))

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

(define (rectangle w h m p)
  (new (Rectangle w h m p)))

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
                          0 0 (twice #js*.Math.PI)))))])

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

    (define width   (max #js.ima.width #js.imb.width))
    (define height  (max #js.ima.height #js.imb.height))

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
        [("middle" "center")  (values 0 0)]))

    (define-values (δ-a-y δ-b-y)
      (case y-place
        [("top")               (values (- ima-cy δ-edge-y)
                                       (- imb-cy δ-edge-y))]
        [("bottom")            (values (- δ-edge-y ima-cy)
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
     (#js.imb.render ctx #js*.this.bDx #js*.this.bDy)
     (#js.ima.render ctx #js*.this.aDx #js*.this.aDy))])

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
