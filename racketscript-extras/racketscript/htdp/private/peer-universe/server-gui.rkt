#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse)
         "encode-decode.rkt"
         "universe-primitives.rkt"
         "../../../private/jscommon.rkt")

(provide server-gui)

(define WIDTH 500)
(define HEIGHT 300)

(define-proto ServerLogger
  (λ (root)
    #:with-this this

    (:= #js.this.logs  ($/array))
    (:= #js.this.autoscroll? #true)

    (:= #js.this.peer-id #js"")

    ;; Create elements
    (:= #js.this.container      (#js*.document.createElement #js"div"))
    (:= #js.this.textbox        (#js*.document.createElement #js"textarea"))
    (:= #js.this.checkbox-div   (#js*.document.createElement #js"div"))
    (:= #js.this.checkbox-label (#js*.document.createElement #js"label"))
    (:= #js.this.checkbox       (#js*.document.createElement #js"input"))
    (:= #js.this.id-text        (#js*.document.createElement #js"em"))
    (:= #js.this.id-copy-button (#js*.document.createElement #js"button"))

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

    (:= #js.this.id-text.innerHTML           #js"peer id: undefined ")
    (:= #js.this.id-copy-button.innerHTML    #js"copy")
    (:= #js.this.id-copy-button.style.margin-left #js"5px")

    (:= #js.this.id-copy-button.onclick
        (λ ()
          (#js*.navigator.clipboard.writeText #js.this.peer-id)
          (#js*.alert #js"Copied peer ID to clipboard.")))

    ;; Add elements to document
    (#js.this.checkbox-div.appendChild #js.this.checkbox-label)
    (#js.this.checkbox-div.appendChild #js.this.checkbox)

    (#js.this.container.appendChild #js.this.id-text)
    (#js.this.container.appendChild #js.this.id-copy-button)
    (#js.this.container.appendChild #js.this.textbox)
    (#js.this.container.appendChild #js.this.checkbox-div)
    (#js.root.appendChild #js.this.container)
    this)
    [log
     (λ (text) 
       #:with-this this
       (#js.this.logs.push (js-string text))
       (#js.this.render)
       (#js*.console.log (js-string text)))]
    [show
     (λ ()
       #:with-this this
       (:= #js.this.container.style.display "block"))]
    [hide
     (λ ()
       #:with-this this
       (:= #js.this.container.style.display #js"none"))]
    [set-id!
     (λ (new-id)
       #:with-this this
       (:= #js.this.peer-id new-id)
       (:= #js.this.id-text.innerHTML (js-string (format "peer id: ~a " new-id))))]
    [render
     (λ ()
       #:with-this this
       (define log-string (#js.this.logs.reduce (λ (res curr)
                                                  (if ($/binop === res #js"")
                                                      (js-string curr)
                                                      ($/+ res #js"\n\n" (js-string curr))))
                                                #js""))
       (:= #js.this.textbox.innerHTML log-string)
       (when (equal? #js.this.autoscroll? #true)
             (:= #js.this.textbox.scrollTop #js.this.textbox.scrollHeight)))])

(define (make-gui root)
  (new (ServerLogger root)))

(define (server-gui [root-element #js*.document.body])
  (make-gui root-element))