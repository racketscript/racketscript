#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse)
         "../private/jscommon.rkt")

(provide on-mouse
         on-tick
         on-key
         on-release
         to-draw
         stop-when
         big-bang

         key=?
         mouse=?)

(define *default-frames-per-second* 70)

(define (make-big-bang init-world handlers)
  (new (BigBang init-world handlers)))

(define (big-bang init-world . handlers)
  ($> (make-big-bang init-world handlers)
      (setup)
      (start)))

(define-proto BigBang
  (λ (init-world handlers)
    #:with-this this
    (:= #js.this.world      init-world)
    (:= #js.this.interval   (/ 1000 *default-frames-per-second*))
    (:= #js.this.handlers   handlers)

    (:= #js.this.-active-handlers         ($/obj))
    (:= #js.this.-world-change-listeners  ($/array))

    (:= #js.this.-idle       #t)
    (:= #js.this.-stopped    #t)
    (:= #js.this.-events     ($/array)))
  [setup
   (λ ()
     #:with-this this
     ;; Create canvas DOM element and add to screen
     (define canvas  (#js.document.createElement #js"canvas"))
     (define ctx     (#js.canvas.getContext #js"2d"))

     (#js.canvas.setAttribute #js"tabindex" 1)
     (#js.canvas.setAttribute #js"style" #js"outline: none")

     (:= #js.this.-canvas    canvas)
     (:= #js.this.-context   ctx)

     (#js.document.body.appendChild canvas)
     (#js.canvas.focus)

     (#js.this.register-handlers)

     ;; Set canvas size as the size of first world
     (define draw-handler ($ #js.this.-active-handlers #js"to-draw"))
     (unless draw-handler
       (error 'big-bang "to-draw handle not provided"))
     (define img ($$ draw-handler.callback #js.this.world))
     (:= #js.canvas.width   #js.img.width)
     (:= #js.canvas.height  #js.img.height)

     ;; We are reassiging using change-world so that change world
     ;; callbacks gets invoked at start of big-bang
     (#js.this.change-world #js.this.world)

     this)]
  [register-handlers
   (λ ()
     #:with-this this
     (define active-handlers #js.this.-active-handlers)
     (let loop ([handlers #js.this.handlers])
       (when (pair? handlers)
         (define h ((car handlers) this))
         (#js.h.register)
         (:= ($ active-handlers #js.h.name) h)
         (loop (cdr handlers)))))]
  [deregister-handlers
   (λ ()
     #:with-this this
     (define active-handlers #js.this.-active-handlers)
     ($> (#js*.Object.keys active-handlers)
         (forEach
          (λ (key)
            (define h ($ active-handlers key))
            (#js.h.deregister)
            (:= ($ #js.active-handlers #js.h.name) *undefined*)))))]
  [start
   (λ ()
     #:with-this this
     (:= #js.this.-stopped #f)
     (#js.this.process-events))]
  [stop
   (λ ()
     #:with-this this
     (#js.this.clear-event-queue)
     (set-object! this
                  [-stopped #t]
                  [-idle    #t])
     (#js.this.deregister-handlers)
     (set-object! #js.this
                  [-active-handlers ($/obj)]
                  [handlers '()]))]
  [clear-event-queue
   (λ ()
     #:with-this this
     (#js.this.-events.splice 0 #js.this.-events.length))]
  [queue-event
   (λ (e)
     #:with-this this
     (#js.this.-events.push e)
     (when #js.this.-idle
       (schedule-animation-frame #js.this 'process_events)))]
  [change-world
   (λ (new-world)
     #:with-this this
     (define listeners #js.this.-world-change-listeners)
     (let loop ([i 0])
       (when (< i #js.listeners.length)
         (define listener ($ #js.listeners i))
         (listener new-world)
         (loop (add1 i))))
     (:= #js.this.world new-world))]
  [add-world-change-listener
   (λ (cb)
     #:with-this this
     (#js.this.-world-change-listeners.push cb))]
  [remove-world-change-listener
   (λ (cb)
     #:with-this this
     (define index (#js.this.-world-change-listeners.indexOf cb))
     (#js.this.-world-change-listeners.splice index 1))]
  [process-events
   (λ ()
     #:with-this this
     (define events #js.this.-events)

     (:= #js.this.-idle #f)

     (let loop ([world-changed? #f])
       (cond
         [(> #js.events.length 0)
          (define evt         (#js.events.shift))
          (define handler     ($ #js.this.-active-handlers #js.evt.type))

          (define changed?
            (cond
              [handler (#js.handler.invoke #js.this.world evt)]
              [(equal? #js.evt.type #js"raw")
               (#js.evt.invoke #js.this.world evt)]
              [else
               (#js.console.warn "ignoring unknown/unregistered event type: " evt)]))
          (loop (or world-changed? changed?))]
         [(and world-changed? (not #js.this.-stopped))
          (#js.this.queue-event ($/obj [type #js"to-draw"]))
          (loop #f)]))

     (:= #js.this.-idle #t))])

(define (to-draw cb)
  (λ (bb)
    (define on-tick-evt ($/obj [type #js"to-draw"]))
    ($/obj
     [name        #js"to-draw"]
     [register    (λ () (void))]
     [deregister  (λ () (void))]
     [callback    cb]
     [invoke      (λ (world evt)
                    (define ctx      #js.bb.-context)
                    (define img      (cb #js.bb.world))
                    (define height   #js.img.height)
                    (define width    #js.img.width)

                    (#js.ctx.clearRect 0 0 width height)
                    (#js.img.render ctx (half width) (half height))

                    #f)])))

(define (on-tick cb rate)
  (λ (bb)
    (define on-tick-evt ($/obj [type #js"on-tick"]))
    ($/obj
     [name         #js"on-tick"]
     [register     (λ ()
                     #:with-this this
                     (#js.bb.queue-event on-tick-evt)
                     (if rate
                         (set! rate (* 1000 rate))
                         (set! rate #js.bb.interval)))]
     [deregister   (λ ()
                     #:with-this this
                     (define last-cb #js.this.last-cb)
                     (when last-cb
                       ;; TODO: This sometimes doesn't work,
                       ;; particularly with high fps, so we need to do
                       ;; something at event loop itself.
                       (#js*.window.clearTimeout last-cb)))]
     [invoke       (λ (world _)
                     #:with-this this
                     (#js.bb.change-world (cb world))
                     (:= #js.this.last-cb (#js*.setTimeout
                                            (λ ()
                                              (#js.bb.queue-event on-tick-evt))
                                            rate))
                     #t)])))

(define (on-mouse cb)
  (λ (bb)
    ($/obj
     [name          #js"on-mouse"]
     [listeners     ($/obj)]
     [register
      (λ ()
        #:with-this this
        (define canvas #js.bb.-canvas)
        (define (make-listener r-evt-name)
          (λ (evt)
            (define posn (canvas-posn-δ canvas evt))
            (#js.bb.queue-event ($/obj [type #js"on-mouse"]
                                       [evt  (js-string->string r-evt-name)]
                                       [x    ($ posn 'x)]
                                       [y    ($ posn 'y)]))))

        (define (register-listener evt-name r-evt-name)
          (define cb (make-listener r-evt-name))
          (#js.canvas.addEventListener evt-name cb)
          (:= ($ #js.this.listeners evt-name) cb))

        (register-listener #js"mousemove"  #js"move")
        (register-listener #js"mousedown"  #js"button-down")
        (register-listener #js"mouseup"    #js"button-up")
        (register-listener #js"mouseout"   #js"leave")
        (register-listener #js"mouseover"  #js"enter")
        (register-listener #js"drag"       #js"drag"))]
     [deregister
      (λ ()
        #:with-this this
        (define (remove-listener evt-name)
          (define cb ($ #js.this.listeners evt-name))
          (#js.bb.-canvas.removeEventListener evt-name cb))
        (remove-listener #js"mousemove")
        (remove-listener #js"mousedown")
        (remove-listener #js"mouseup")
        (remove-listener #js"mouseout")
        (remove-listener #js"mouseover")
        (remove-listener #js"drag"))]
     [invoke
      (λ (world evt)
        (define new-world (cb world #js.evt.x #js.evt.y #js.evt.evt))
        (#js.bb.change-world new-world)
        #t)])))

(define-syntax-rule (-on-key-* r-evt-name evt-name)
  (λ (cb)
    (λ (bb)
      ($/obj
       [name        r-evt-name]
       [register
        (λ ()
          #:with-this this
          (define canvas #js.bb.-canvas)
          (:= #js.this.listener
              (λ (evt)
                (#js.evt.preventDefault)
                (#js.evt.stopPropagation)
                (#js.bb.queue-event ($/obj [type r-evt-name]
                                           [key  (key-event->key-name evt)]))))
          (#js.canvas.addEventListener evt-name #js.this.listener))]
       [deregister
        (λ ()
          #:with-this this
          (#js.bb.-canvas.removeEventListener evt-name #js.this.listener)
          (:= #js.this.listener *undefined*))]
       [invoke
        (λ (world evt)
          (define new-world (cb world #js.evt.key))
          (#js.bb.change-world new-world)
          #t)]))))

(define on-key     (-on-key-* #js"on-key" #js"keydown"))
(define on-release (-on-key-* #js"on-release" #js"keyup"))

(define (stop-when last-world? [last-picture #f])
  (λ (bb)
    ($/obj
     [name         #js"stop-when"]
     [predicate    last-world?]
     [lastpicture  last-picture]
     [register
      (λ ()
        #:with-this this
        (#js.bb.add-world-change-listener #js.this.invoke))]
     [deregister
      (λ ()
        #:with-this this
        (#js.bb.remove-world-change-listener #js.this.invoke))]
     [invoke
      (λ (w)
        (when (last-world? w)
          (#js.bb.stop)
          (when last-picture
            (define handler ((to-draw last-picture) bb))
            (#js.bb.queue-event
             ($/obj [type       #js"raw"]
                    [invoke     #js.handler.invoke])))))])))

;; maps JS KeyboardEvent.key to big-bang KeyEvent
(define key-table
  ($/obj  [Backspace "\b"]
          [Enter "\r"]
          [Tab "\t"]
          [ArrowLeft "left"]
          [ArrowRight "right"]
          [ArrowDown "down"]
          [ArrowUp "up"]
          [Shift "shift"]
          [Control "control"]
          [ControlRight "rcontrol"]
          [ControlLeft "control"]
          [ShiftRight "rshift"]
          [ShiftLeft "shift"]
          [Escape "escape"]
          [Home "home"]
          [End "end"]
          [Insert "insert"] ; no pageup/down in big-bang?
          [Delete "\u007F"] ; rubout
          [Pause "pause"]
          [NumLock "numlock"]
          [F1 "f1"]
          [F2 "f2"]
          [F3 "f3"]
          [F4 "f4"]
          [F5 "f5"]
          [F6 "f6"]
          [F7 "f7"]
          [F8 "f8"]
          [F9 "f9"]
          [F10 "f10"]
          [F11 "f11"]
          [F12 "f12"]
          ; unsure about these big bang KeyEvents:
          ;; "start"
          ;; "cancel"
          ;; "clear"
          ;; "menu"
          ;; "capital"
          ;; "prior"
          ;; "next"
          ;; "select"
          ;; "print"
          ;; "execute"
          ;; "snapshot"
          ;; "help"
          ;; "scroll"
          ))

(define (key-event->key-name e)
  (define k #js.e.key)
  (define code ; use .code to differentiate left/right shift, ctrl, alt
    (if (or ($/binop === k #js"Shift") ($/binop === k #js"Control") ($/binop === k #js"Alt"))
        #js.e.code
        k))
  (let ([key-table-code ($ key-table code)])
       (if (void? key-table-code)
           (js-string->string code)
           key-table-code)))

(define (canvas-posn-δ canvas evt)
  (define rect (#js.canvas.getBoundingClientRect))
  ($/obj
   [x (- #js.evt.clientX #js.rect.left)]
   [y (- #js.evt.clientY #js.rect.top)]))

(define (key=? k1 k2)
  (equal? k1 k2))
(define (mouse=? m1 m2)
  (equal? m1 m2))
