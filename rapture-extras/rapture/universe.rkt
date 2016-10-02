#lang rapture/base

(require (for-syntax rapture/base
                     syntax/parse)
         "jscommon.rkt")

(provide on-mouse
         on-tick
         on-key
         to-draw
         big-bang)

(define (make-big-bang init-world handlers)
  (new (BigBang init-world handlers)))

(define (big-bang init-world . handlers)
  ($> (make-big-bang init-world handlers)
      (setup)
      (start)))

(define-proto BigBang
  #:init
  (λ (init-world handlers)
    (:= #js*.this.world      init-world)
    (:= #js*.this.interval   (/ 1000 120))
    (:= #js*.this.handlers   handlers)
    (:= #js*.this._activeHandlers  ($/obj))

    (:= #js*.this.checked    #t)
    (:= #js*.this.paused     #t)

    (:= #js*.this._events    ($/array)))

  #:prototype-fields
  [setup
   (λ ()
     ;; Create canvas DOM element and add to screen
     (define canvas  (#js.document.createElement "canvas"))
     (define ctx     (#js.canvas.getContext "2d"))

     (#js.canvas.setAttribute "tabindex" 1)
     (#js.canvas.focus)

     (:= #js*.this._canvas    canvas)
     (:= #js*.this._context   ctx)

     (#js.document.body.appendChild canvas)
     (#js*.this.registerHandlers)
     #js*.this)]
  [registerHandlers
   (λ ()
     (define activeHandlers #js*.this._activeHandlers)
     (define self #js*.this)
     (let loop ([handlers #js*.this.handlers])
       (when (pair? handlers)
         (define h ((car handlers) self))
         (#js.h.register)
         (:= ($ activeHandlers #js.h.name) h)
         (loop (cdr handlers)))))]
  [start
   (λ ()
     (#js*.this._processEvents))]
  [_processEvents
   (λ ()
     (define events #js*.this._events)
     (define self #js*.this)

     (let loop ([world-changed? #f])
       (cond
         [(> #js.events.length 0)
          (define evt (#js.events.shift))
          (define callback ($ #js.self._activeHandlers #js.evt.type "callback"))
          (unless callback
            (error 'big-bang "invalid event"))
          (loop (callback #js.self.world evt))]
         [world-changed?
          (#js.events.push ($/obj [type "to-draw"]))
          (loop #f)]
         [(schedule-method self '_processEvents #js.self.interval)])))])


(define (on-tick cb rate)
  (λ (bb)
    (define on-tick-evt ($/obj [type "on-tick"]))
    ($/obj
     [name         "on-tick"]
     [register     (λ ()
                     (#js.bb._events.push on-tick-evt)
                     (unless rate
                       (set! rate #js.bb.interval)))]
     [deregister   (λ () (void))]
     [callback     (λ (world _)
                     (#js*.setTimeout (λ ()
                                        (#js.bb._events.push on-tick-evt))
                                      rate)
                     #t)])))

(define (to-draw cb)
  (λ (bb)
    (define on-tick-evt ($/obj [type "to-draw"]))
    ($/obj
     [name        "to-draw"]
     [register    (λ () (void))]
     [deregister  (λ () (void))]
     [callback    (λ (world evt)
                    (define ctx      #js.bb._context)
                    (define img      (cb #js.bb.world))
                    (define height   #js.img.width)
                    (define width    #js.img.height)

                    (:= #js.bb._canvas.width   width)
                    (:= #js.bb._canvas.height  height)

                    (#js.ctx.clearRect 0 0 width height)
                    (#js.img.render ctx (half width) (half height))
                    #f)])))

(define (on-mouse cb)
  (λ (bb)
    ($/obj
     [name          "on-mouse"]
     [register
      (λ ()
        (define canvas #js.bb._canvas)
        (define (register-event evt-name r-evt-name)
          (#js.canvas.addEventListener
           evt-name
           (λ (evt)
             (define posn (canvas-posn-δ canvas evt))
             (#js.bb._events.push ($/obj [type "on-mouse"]
                                         [evt  r-evt-name]
                                         [x    ($ posn 'x)]
                                         [y    ($ posn 'y)])))))

        (register-event "mousemove"  "move")
        (register-event "mousedown"  "button-down")
        (register-event "mouseup"    "button-up")
        (register-event "mouseout"   "leave")
        (register-event "mouseover"  "enter")
        (register-event "drag"       "drag"))]
     [deregister
      (λ ()
        (void))]
     [callback
      (λ (world evt)
        (define new-world (cb world #js.evt.x #js.evt.y #js.evt.evt))
        (:= #js.bb.world new-world)
        #t)])))

(define (on-key cb)
  (λ (bb)
    ($/obj
     [name        "on-key"]
     [register
      (λ ()
        (define canvas #js.bb._canvas)
        (#js.canvas.addEventListener
         "keydown"
         (λ (evt)
           (define posn (canvas-posn-δ canvas evt))
           (#js.evt.preventDefault)
           (#js.evt.stopPropagation)
           (#js.bb._events.push ($/obj [type "on-key"]
                                       [key  (key-event->key-name evt)])))
         #t))]
     [deregister (λ () (void))]
     [callback
      (λ (world evt)
        (#js.console.log evt)
        (define new-world (cb world #js.evt.key))
        (:= #js.bb.world new-world)
        #t)])))

;; TODO: A JS object would be faster.
(define *key-table*
  (hasheq 16 "shift"
          17 "control"
          19 "pause"
          27 "escape"
          33 "prior"
          34 "next"
          35 "end"
          36 "home"
          37 "left"
          38 "up"
          39 "right"
          40 "down"
          42 "print"
          45 "insert"
          106 "*"
          107 "+"
          109 "-"
          110 "."
          111 "/"
          144 "numlock"
          145 "scroll"
          186 ";"
          187 "="
          188 ","
          189 "-"
          190 "."
          191 "/"
          192 "`"
          219 "["
          220 "\\"
          221 "]"
          22  "'"))

(define (key-event->key-name e)
  (define code (or #js.e.charCode #js.e.keyCode))
  (or (hash-ref *key-table* code #f)
      (cond
        [(<= 96 code 105)
         ($> (- code 96) (toString))]
        [(<= 112 code 123)
         (++ "f" ($> (- code 111) (toString)))]
        [else
         ($> (#js*.String.fromCharCode code)
             (toLowerCase))])))

(define (canvas-posn-δ canvas evt)
  (define rect (#js.canvas.getBoundingClientRect))
  ($/obj
   [x (- #js.evt.clientX #js.rect.left)]
   [y (- #js.evt.clientY #js.rect.top)]))
