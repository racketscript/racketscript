#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse)
         "encode-decode.rkt"
         "debug-tools.rkt"
         "universe-primitives.rkt"
         "jscommon.rkt")

(provide server-gui)

(define DEFAULT-DISPLAY-MODE #js"block")
(define WIDTH 500)
(define HEIGHT 300)

(define-proto ServerLogger
  (λ (root stop-callback restart-callback)
    #:with-this this

    ; <div id="server-logger-container">
    ;     <checkbox>Auto-scroll</checkbox>
    ;     <textbox>logged text</textbox>
    ;     <div class="button-container">
    ;          <button>stop</button>
    ;         <button>stop and restart</button>
    ;     </div>
    ; </div>
    (:= #js.this.logs  ($/array))
    (:= #js.this.autoscroll? #true)

    ;; Create elements
    (:= #js.this.container      (#js*.document.createElement #js"div"))
    (:= #js.this.textbox        (#js*.document.createElement #js"textarea"))
    (:= #js.this.checkbox-div   (#js*.document.createElement #js"div"))
    (:= #js.this.checkbox-label (#js*.document.createElement #js"label"))
    (:= #js.this.checkbox       (#js*.document.createElement #js"input"))
    (:= #js.this.button-div     (#js*.document.createElement #js"div"))
    (:= #js.this.stop-button    (#js*.document.createElement #js"button"))
    (:= #js.this.restart-button (#js*.document.createElement #js"button"))

    ;; Configure elements
    (:= #js.this.container.style.display #js"none")
    (:= #js.this.container.style.width  (js-string (format "~apx" WIDTH)))
    (:= #js.this.container.style.height (js-string (format "~apx" HEIGHT)))
    
    (:= #js.this.textbox.style.width #js"inherit")
    (:= #js.this.textbox.style.height #js"inherit")

    (:= #js.this.checkbox-label.for #js"autoscroll")
    (:= #js.this.checkbox-label.innerHTML #js"autoscroll with new input")
    (:= #js.this.checkbox.type #js"checkbox")
    (:= #js.this.checkbox.onclick (lambda () (:= #js.this.autoscroll? #js.this.checkbox.checked)))
    (:= #js.this.checkbox.checked #true)

    (:= #js.this.stop-button.innerHTML          #js"stop")
    (:= #js.this.stop-button.style.grid-area    #js"stop")
    (:= #js.this.stop-button.onclick            stop-callback)
    (:= #js.this.restart-button.innerHTML       #js"restart")
    (:= #js.this.restart-button.style.grid-area #js"restart")
    (:= #js.this.restart-button.onclick         restart-callback)
    (:= #js.this.button-div.style.width         #js"100%")
    (:= #js.this.button-div.style.display       #js"grid")
    (:= #js.this.button-div.style.gridTemplateAreas
        #js"'stop restart'")

    ;; Add elements to document
    (#js.this.checkbox-div.appendChild #js.this.checkbox-label)
    (#js.this.checkbox-div.appendChild #js.this.checkbox)

    (#js.this.button-div.appendChild #js.this.stop-button)
    (#js.this.button-div.appendChild #js.this.restart-button)

    (#js.this.container.appendChild #js.this.textbox)
    (#js.this.container.appendChild #js.this.checkbox-div)
    (if (and restart-callback stop-callback)
        (#js.this.container.appendChild #js.this.button-div)
        (void))
    (#js.root.appendChild #js.this.container)
    this)
    [log
     (λ (text) 
       #:with-this this
       (#js.this.logs.push (js-string text))
       (#js.this.render)
       (#js*.console.log (js-string text))
       (void))]
    [show
     (λ ()
       #:with-this this
       (:= #js.this.container.style.display DEFAULT-DISPLAY-MODE)
       (void))]
    [hide
     (λ ()
       #:with-this this
       (:= #js.this.container.style.display #js"none")
       (void))]
    [render
     (λ ()
       #:with-this this
       (define log-string (#js.this.logs.reduce (λ (res curr)
                                                  (if ($/binop === res #js"")
                                                      (js-string curr)
                                                      ($/+ res #js"\n\n" (js-string curr))))
                                                #js""))
       (:= #js.this.textbox.innerHTML log-string)
       (cond [(equal? #js.this.autoscroll? #true)
              (:= #js.this.textbox.scrollTop #js.this.textbox.scrollHeight)]
             [else (void)])
       (void))])

(define (make-gui root stop-callback restart-callback)
  (new (ServerLogger root stop-callback restart-callback)))

(define (server-gui [root-element #js*.document.body] [stop-callback #false] [restart-callback #false])
  (make-gui root-element stop-callback restart-callback))