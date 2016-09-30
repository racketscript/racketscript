#lang rapture/base

(require (for-syntax rapture/base
                     syntax/parse))

;;-----------------------------------------------------------------------------
;; Interop helpers

(define-syntax  :=        (make-rename-transformer #'$/:=))
(define-syntax  new       (make-rename-transformer #'$/new))

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

(define (make-line x y pen-or-color)
  ($/obj
   [type    "line"]
   [width   (abs+ceil x)]
   [height  (abs+ceil y)]
   [mode    #f]
   [color   pen-or-color]
   [render (λ (ctx x y)
             (with-origin ctx [x y]
               (with-path ctx {"outline" pen-or-color}
                 (define sx (- (abs (half x))))
                 (define sy (- (abs (half y))))
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
                    (#js.ctx.lineTo sx sy)]))))]))

(define (make-rectangle width height mode pen-or-color)
  ($/obj
   [type     "rectangle"]
   [width    width]
   [height   height]
   [mode     mode]
   [pen      pen-or-color]
   [render
    (λ (ctx x y)
      (with-origin ctx [x y]
        (with-path ctx {mode pen-or-color}
          (let ([start-x   (- (half width))]
                [start-y   (- (half height))])
            (#js.ctx.rect start-x start-y width height)))))]))

(define (make-circle radius mode pen-or-color)
  (define diameter (twice radius))
  ($/obj
   [type     "circle"]
   [radius   radius]
   [width    diameter]
   [height   diameter]
   [mode     mode]
   [pen      pen-or-color]
   [render
    (λ (ctx x y)
      (with-origin ctx [x y]
        (with-path ctx {mode pen-or-color}
          (#js.ctx.ellipse 0 0            ;; center
                           radius radius  ;; radius-x, radius-y
                           0 0 (twice #js*.Math.PI)))))]))

;;-----------------------------------------------------------------------------
;; Combine images

(define (make-overlay x-place y-place ima imb)
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

  ($/obj
   [type       "overlay"]
   [first      ima]
   [second     imb]
   [width      width]
   [height     height]
   [render
    (λ (ctx x y)
      (#js.imb.render ctx δ-b-x δ-b-y)
      (#js.ima.render ctx δ-a-x δ-a-y))]))

(define (make-container-image childs posns width height)
  ($/obj
   [type     "container"]
   [childs   childs]
   [posns    posns]
   [width    width]
   [height   height]
   [render
    (λ (ctx x y)
      (with-origin ctx [(- x (half width)) (- y (half height))]
        (#js.ctx.beginPath)
        (#js.ctx.rect 0 0 (sub1 width) (sub1 height))
        (#js.ctx.clip)
        (let loop ([childs childs]
                   [posns  posns])
          (unless (null? childs)
            (define child (car childs))
            (define posn (car posns))
            (#js.child.render ctx #js.posn.x #js.posn.y)
            (loop (cdr childs) (cdr posns))))))]))

(define (make-place-image child cx cy base)
  (let ([width #js.base.width]
        [height #js.base.height])
    (make-container-image (list base child)
                          (list (posn (half width) (half height))
                                (posn cx cy))
                          #js.base.width
                          #js.base.height)))
