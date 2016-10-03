#lang rapture/base

(require (for-syntax rapture/base
                     syntax/parse)
         "../private/jscommon.rkt")

(provide on-mouse
         on-tick
         on-key
         to-draw
         stop-when
         big-bang

         key=?)

(define *default-frames-per-second* 70)

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
    (:= #js*.this.interval   (/ 1000 *default-frames-per-second*))
    (:= #js*.this.handlers   handlers)

    (:= #js*.this._activeHandlers        ($/obj))
    (:= #js*.this._worldChangeListeners  ($/array))

    (:= #js*.this._idle       #t)
    (:= #js*.this._stopped    #t)
    (:= #js*.this._events    ($/array)))

  #:prototype-fields
  [setup
   (λ ()
     ;; Create canvas DOM element and add to screen
     (define canvas  (#js.document.createElement "canvas"))
     (define ctx     (#js.canvas.getContext "2d"))

     (#js.canvas.setAttribute "tabindex" 1)

     (:= #js*.this._canvas    canvas)
     (:= #js*.this._context   ctx)

     (#js.document.body.appendChild canvas)
     (#js.canvas.focus)

     (#js*.this.registerHandlers)

     ;; Set canvas size as the size of first world
     (define draw-handler ($ #js*.this._activeHandlers "to-draw"))
     (unless draw-handler
       (error 'big-bang "to-draw handle not provided"))
     (define img ($$ draw-handler.callback #js*.this.world))
     (:= #js.canvas.width   #js.img.width)
     (:= #js.canvas.height  #js.img.height)

     ;; We are reassiging using changeWorld so that change world
     ;; callbacks gets invoked at start of big-bang
     (#js*.this.changeWorld #js*.this.world)

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
  [deregisterHandlers
   (λ ()
     (define activeHandlers #js*.this._activeHandlers)
     (define self #js*.this)
     ($> (#js*.Object.keys activeHandlers)
         (forEach
          (λ (key)
            (define h ($ activeHandlers key))
            (#js.h.deregister)
            (:= ($ #js.activeHandlers #js.h.name) #js*.undefined)))))]
  [start
   (λ ()
     (:= #js*.this._stopped #f)
     (#js*.this.processEvents))]
  [stop
   (λ ()
     (#js*.this.clearEventQueue)
     (set-object! #js*.this
                  [_stopped #t]
                  [_idle    #t])
     (#js*.this.deregisterHandlers)
     (set-object! #js*.this
                  [_activeHandlers ($/obj)]
                  [handlers '()]))]
  [clearEventQueue
   (λ ()
     (#js*.this._events.splice 0 #js*.this._events.length))]
  [queueEvent
   (λ (e)
     (#js*.this._events.push e)
     (when #js*.this._idle
       (schedule-animation-frame #js*.this 'processEvents)))]
  [changeWorld
   (λ (new-world)
     (define listeners #js*.this._worldChangeListeners)
     (let loop ([i 0])
       (when (< i #js.listeners.length)
         (define listener ($ #js.listeners i))
         (listener new-world)
         (loop (add1 i))))
     (:= #js*.this.world new-world))]
  [addWorldChangeListener
   (λ (cb)
     (#js*.this._worldChangeListeners.push cb))]
  [removeWorldChangeListener
   (λ (cb)
     (define index (#js*.this._worldChangeListeners.indexOf cb))
     (#js*.this._worldChangeListeners.splice index 1))]
  [processEvents
   (λ ()
     (define events #js*.this._events)
     (define self #js*.this)

     (:= #js*.this._idle #f)

     (let loop ([world-changed? #f])
       (cond
         [(> #js.events.length 0)
          (define evt         (#js.events.shift))
          (define handler     ($ #js.self._activeHandlers #js.evt.type))

          (define changed?
            (cond
              [handler (#js.handler.invoke #js.self.world evt)]
              [(equal? #js.evt.type "raw")
               (#js.evt.invoke #js.self.world evt)]
              [else
               (#js.console.warn "ignoring unknown/unregistered event type: " evt)]))
          (loop (or world-changed? changed?))]
         [(and world-changed? (not #js.self._stopped))
          (#js.self.queueEvent ($/obj [type "to-draw"]))
          (loop #f)]))

     (:= #js*.this._idle #t))])

(define (to-draw cb)
  (λ (bb)
    (define on-tick-evt ($/obj [type "to-draw"]))
    ($/obj
     [name        "to-draw"]
     [register    (λ () (void))]
     [deregister  (λ () (void))]
     [callback    cb]
     [invoke      (λ (world evt)
                    (define ctx      #js.bb._context)
                    (define img      (cb #js.bb.world))
                    (define height   #js.img.height)
                    (define width    #js.img.width)

                    (#js.ctx.clearRect 0 0 width height)
                    (#js.img.render ctx (half width) (half height))

                    #f)])))

(define (on-tick cb rate)
  (λ (bb)
    (define on-tick-evt ($/obj [type "on-tick"]))
    ($/obj
     [name         "on-tick"]
     [register     (λ ()
                     (#js.bb.queueEvent on-tick-evt)
                     (if rate
                         (set! rate (* 1000 rate))
                         (set! rate #js.bb.interval)))]
     [deregister   (λ ()
                     (define lastcb #js*.this.lastcb)
                     (when lastcb
                       ;; TODO: This sometimes doesn't work,
                       ;; particularly with high fps, so we need to do
                       ;; something at event loop itself.
                       (#js*.window.clearTimeout lastcb)))]
     [invoke       (λ (world _)
                     (#js.bb.changeWorld (cb world))
                     (:= #js*.this.lastcb (#js*.setTimeout
                                           (λ ()
                                             (#js.bb.queueEvent on-tick-evt))
                                           rate))
                     #t)])))

(define (on-mouse cb)
  (λ (bb)
    ($/obj
     [name          "on-mouse"]
     [listeners     ($/obj)]
     [register
      (λ ()
        (define canvas #js.bb._canvas)
        (define (make-listener r-evt-name)
          (λ (evt)
            (define posn (canvas-posn-δ canvas evt))
            (#js.bb.queueEvent ($/obj [type "on-mouse"]
                                      [evt  r-evt-name]
                                      [x    ($ posn 'x)]
                                      [y    ($ posn 'y)]))))

        (define self #js*.this) ;; TODO: is this needed?
        (define (register-listener evt-name r-evt-name)
          (define cb (make-listener r-evt-name))
          (#js.canvas.addEventListener evt-name cb)
          (:= ($ #js.self.listeners evt-name) cb))

        (register-listener "mousemove"  "move")
        (register-listener "mousedown"  "button-down")
        (register-listener "mouseup"    "button-up")
        (register-listener "mouseout"   "leave")
        (register-listener "mouseover"  "enter")
        (register-listener "drag"       "drag"))]
     [deregister
      (λ ()
        (define self #js*.this)
        (define (remove-listener evt-name)
          (define cb ($ #js.self.listeners evt-name))
          (#js.bb._canvas.removeEventListener evt-name cb))
        (remove-listener "mousemove")
        (remove-listener "mousedown")
        (remove-listener "mouseup")
        (remove-listener "mouseout")
        (remove-listener "mouseover")
        (remove-listener "drag"))]
     [invoke
      (λ (world evt)
        (define new-world (cb world #js.evt.x #js.evt.y #js.evt.evt))
        (#js.bb.changeWorld new-world)
        #t)])))

(define (on-key cb)
  (λ (bb)
    ($/obj
     [name        "on-key"]
     [register
      (λ ()
        (define canvas #js.bb._canvas)
        (:= #js*.this.listener
            (λ (evt)
              (#js.evt.preventDefault)
              (#js.evt.stopPropagation)
              (#js.bb.queueEvent ($/obj [type "on-key"]
                                        [key  (key-event->key-name evt)]))))
        (#js.canvas.addEventListener "keydown" #js*.this.listener))]
     [deregister
      (λ ()
        (#js.bb._canvas.removeEventListener "keydown" #js*.this.listener)
        (:= #js*.this.listener #js*.undefined))]
     [invoke
      (λ (world evt)
        (define new-world (cb world #js.evt.key))
        (#js.bb.changeWorld new-world)
        #t)])))

(define (stop-when last-world? [last-picture #f])
  (λ (bb)
    ($/obj
     [name         "stop-when"]
     [predicate    last-world?]
     [lastpicture  last-picture]
     [register
      (λ ()
        (#js.bb.addWorldChangeListener #js*.this.invoke))]
     [deregister
      (λ ()
        (#js.bb.removeWorldChangeListener #js*.this.invoke))]
     [invoke
      (λ (w)
        (when (last-world? w)
          (#js.bb.stop)
          (when last-picture
            (define handler ((to-draw last-picture) bb))
            (#js.bb.queueEvent
             ($/obj [type       "raw"]
                    [invoke     #js.handler.invoke])))))])))


;; TODO: A JS object would be faster.
(define *key-table*
  (hasheq 16 "shift"
          17 "controxl"
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

(define (key=? k1 k2)
  (equal? k1 k2))
