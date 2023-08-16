#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse)
         "server-gui.rkt"
         "encode-decode.rkt"
         "debug-tools.rkt"
         "universe-primitives.rkt"
         "jscommon.rkt"
         "util.rkt")

; TODO:
; implement deregister for on-msg handler
; implement the following handlers
; - to-string
; - check-with
; - state

; Variations from api:
; - no port handler
; - create clause for user to pass in
;   root element for logging GUI

; Add to logs:
; u: current universe state
; Events to log:
; - mail sending:
;   "broadcast failed to ~a" iworld name
;   "~s not on the list" iworld name

(provide universe

         on-new
         on-msg
         on-disconnect
         server-id
         
        ;  peerjs
         Peer)

;; Adds peerjs exports (primarily Peer constructor) to window object
(define peerjs ($/require "https://cdnjs.cloudflare.com/ajax/libs/peerjs/1.4.7/peerjs.min.js" *))

(define Peer #js*.window.Peer)

(define DEFAULT-UNIVERSE-ID "server") ;; Change this

(define *default-frames-per-second* 70)

;; Universe server
(define (make-universe init-state handlers gui-root)
  (new (Universe init-state handlers (if ($/binop != gui-root $/null)
                                         gui-root #js*.document.body))))

(define (universe init-state #:dom-root [gui-root $/null] . handlers)
  ($> (make-universe init-state handlers gui-root)
      (setup)
      (start)))

(define-proto Universe
  (λ (init-state handlers gui-root)
    #:with-this this
    (:= #js.this.state      init-state)
    (:= #js.this.interval   (/ 1000 *default-frames-per-second*))
    (:= #js.this.handlers   handlers)

    (:= #js.this.is-universe? #true)

    (:= #js.this.gui (server-gui gui-root
                                ;  #js.this.stop
                                ;  (λ () ($> #js.this.stop #js.this.setup #js.this.start))
                                 )) ;; TODO: allow user to pass root element? & Fix stop/restart cb's

    (:= #js.this.-active-handlers         ($/obj))
    (:= #js.this.-state-change-listeners  ($/array))
    (:= #js.this.-message-listeners       ($/array))

    (:= #js.this.-peer            $/undefined)
    (:= #js.this.-peer-init-tasks ($/array))
    (:= #js.this.-active-iworlds ($/array))
    (:= #js.this.-disconnect-tasks ($/array))

    (:= #js.this.-peer-id DEFAULT-UNIVERSE-ID)

    (:= #js.this.-idle       #t)
    (:= #js.this.-stopped    #t)
    (:= #js.this.-events     ($/array)))
  [setup
   (λ ()
     #:with-this this
     (#js.this.register-handlers)
     (#js.this.gui.show)

     (define (log-connection conn)
       (#js.this.gui.log (format "~a signed up" (js-string->string #js.conn.label))))
     (define (log-new-msg iw data)
       (#js.this.gui.log (format "~a --> universe:\n<~a>"
                                        (iworld-name iw) (msg->string (decode-data data)))))

     (#js.this.add-peer-init-task (λ (peer)
                                    (#js.peer.on #js"connection"
                                                 log-connection)))
     (#js.this.-message-listeners.push log-new-msg)
     this)]
  [start
   (λ ()
     #:with-this this
     (#js.this.init-peer-connection)
     (#js.this.gui.log (format "a new universe is up and running with id ~s" 
                               (js-string->string #js.this.-peer.id)))
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
  [stop
   (λ ()
     #:with-this this
     (#js.this.gui.log "stopping the universe\n----------------------------------")
     (void))]
  [clear-event-queue
   (λ ()
     #:with-this this
     (#js.this.-events.splice 0 #js.this.-events.length))]
  [add-state-change-listener
   (λ () 0)]
  [remove-state-change-listener
   (λ () 0)]
  [queue-event
   (λ (e)
     #:with-this this
     (#js.this.-events.push e)
     (when #js.this.-idle
       (schedule-animation-frame #js.this 'process_events)))]
  [process-events
   (λ ()
      #:with-this this
      (define events #js.this.-events)
 
      (:= #js.this.-idle #f)
 
      (let loop ([state-changed? #f])
        (cond
          [(> #js.events.length 0)
           (define evt         (#js.events.shift))
           (define handler     ($ #js.this.-active-handlers #js.evt.type))
           (define changed?
             (cond
               ; raw evt must be checked 1st; bc handler will be undefined
               [(equal? #js.evt.type #js"raw")
                (#js.evt.invoke #js.this.state evt)]
               [(not ($/typeof handler "undefined"))
                (#js.handler.invoke #js.this.state evt)]
               [else
                (#js.console.warn "ignoring unknown/unregistered event type: " evt)]))
           (loop (or state-changed? changed?))]))
 
      (:= #js.this.-idle #t))]
    [change-state
     (λ (result-bundle)
       #:with-this this
       
       (define new-state (bundle-state result-bundle))
       (define mails (bundle-mails result-bundle))
       (define low-to-remove (bundle-low-to-remove result-bundle))

       ;; Send all mails
       (for-each (lambda (curr-mail)
                   (define iworld (mail-to curr-mail))
                   (define conn (iworld-conn iworld))
                   (#js.conn.send (encode-data (mail-content curr-mail)))
                   (#js.this.gui.log (format "universe --> ~a:\n<~a>" 
                                             (iworld-name iworld)
                                             (mail-content curr-mail))))
                 mails)

       ;; Remove all worlds in low-to-remove
       (for-each (lambda (iw)
                   (define conn (iworld-conn iw))
                   (define index (#js.this.-active-iworlds.indexOf iw))
                   (#js.conn.close)
                   (if (> index -1)
                       (#js.this.-active-iworlds.splice index 1)
                       (void)))
                  low-to-remove)

       (define listeners #js.this.-state-change-listeners)
       (let loop ([i 0])
         (when (< i #js.listeners.length)
           (define listener ($ #js.listeners i))
           (listener new-state)
           (loop (add1 i))))
       (:= #js.this.state new-state)
      ;  (#js.this.gui.log (format "~a" new-state))
      ;; Maybe implement this?
       )]
    [init-peer-connection
     (λ (id)
       #:with-this this
       (define peer (new (Peer #js.this.-peer-id)))
       (:= #js.this.-peer peer)
       (#js.peer.on #js"open"
         (λ ()
           (define init-tasks #js.this.-peer-init-tasks)
           (let loop ([i 0])
            (when (< i #js.init-tasks.length)
             (define task ($ #js.init-tasks i))
             (task peer)
             (loop (add1 i)))))))]
    [add-peer-init-task
     (λ (cb) ;; cb = (peer: Peer) => void
       #:with-this this
       ;; If peer already exists, execute callback
       ;; else, append callback to this.-peer-init-tasks[]
       (define peer #js.this.-peer)
       (define peer-started? (not ($/typeof peer "undefined")))
       
       (if peer-started?
           (cb peer)
           (#js.this.-peer-init-tasks.push cb)))]
    [pass-message ;; Passes sender iworld and message to this.-message-listeners
     (λ (sender-iw data)
       #:with-this this
       ;; TODO: Decrypt data once encryption/decryption of racket types solved
       (#js.this.-message-listeners.forEach
         (λ (cb) (cb sender-iw data))))]
    [handle-disconnect
     (λ (iw)
       #:with-this this
       ;; Run all disconnect tasks, passing in the iworld of the connection being closed
       (define tasks #js.this.-disconnect-tasks)
       (let loop ([i 0])
            (when (< i #js.tasks.length)
             (define task ($ tasks i))
             (task iw)
             (loop (add1 i))))
       (#js.this.gui.log (format "~a !! closed port" (iworld-name iw)))
       (void))])

(define (on-new cb)
  (λ (u)
    (define on-new-evt ($/obj [type #js"on-new"]))
    ($/obj
     [name         #js"on-new"]
     [register     (λ ()
                     #:with-this this
                     (define (init-task peer)
                       (define (handle-connection conn)
                         (define name "client name")
                         (if #js.conn.label
                             (set! name (js-string->string #js.conn.label))
                             (void))
                         (define iw (make-iworld conn name))
                         (#js.u.-active-iworlds.push iw)
                         (#js.u.queue-event ($/obj [type #js"on-new"]
                                                   [iWorld iw]))
                         (#js.conn.on #js"close"
                           (λ ()
                             (#js.u.handle-disconnect iw)))
                         (#js.conn.on #js"data"
                           (λ (data) (#js.u.pass-message iw data))))
                       (#js.peer.on #js"connection" handle-connection))
                     
                     (#js.u.add-peer-init-task init-task)

                     (void))]
     [deregister   (λ () ;; TODO: implement this
                     #:with-this this
                     (void))]
     [invoke       (λ (state evt)
                     #:with-this this
                     (define conn (iworld-conn #js.evt.iWorld))
                     (#js.conn.on #js"open"
                                  (λ (_)
                                    (#js.u.change-state 
                                     (cb state #js.evt.iWorld))))
                     #t)])))

(define (on-disconnect cb)
  (λ (u)
    (define on-disconnect-evt ($/obj [type #js"on-disconnect"]))
    ($/obj
     [name         #js"on-disconnect"]
     [register     (λ ()
                     #:with-this this
                     (#js.u.-disconnect-tasks.push
                       (λ (iworld)
                         (#js.u.queue-event ($/obj [type #js"on-disconnect"]
                                                   [iWorld iworld]))))
                     (void))]
     [deregister   (λ () ; TODO: implement this? maybe?
                     #:with-this this
                     (void))]
     [invoke       (λ (state evt)
                     #:with-this this
                     (#js.u.change-state (cb state #js.evt.iWorld))
                     (void))])))

(define (server-id id)
  (λ (u)
    ($/obj
     [name         #js"server-id"]
     [register     (λ ()
                     #:with-this this
                     (:= #js.u.-peer-id (js-string id))
                     (void))]
     [deregister   (λ ()
                     #:with-this this
                     (void))])))

(define (on-msg cb)
  (λ (u)
    (define on-msg-evt ($/obj [type #js"on-msg"]))
    ($/obj
     [name         #js"on-msg"]
     [register     (λ ()
                     #:with-this this
                     (define (handle-msg sender data)
                       (#js.u.queue-event ($/obj [type #js"on-msg"]
                                                 [iWorld sender]
                                                 [msg data])))
                     (#js.u.-message-listeners.push handle-msg)
                     (void))]
     [deregister   (λ () ;; TODO: implement this
                     #:with-this this
                     (void))]
     [invoke       (λ (state evt)
                     (#js.u.change-state (cb state #js.evt.iWorld (decode-data #js.evt.msg)))
                     #t)])))