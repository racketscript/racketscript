#lang racketscript/base

(require (for-syntax racketscript/base
                     syntax/parse)
         "jscommon.rkt"
         "util.rkt")

(provide create-login-form)


;; 
;; User login UI
;; 

(define (create-login-form big-bang-callback universe-callback [root #js*.document.body])
  (define container         (#js*.document.createElement #js"div"))
  
  (define join-form         (#js*.document.createElement #js"form"))
  (define name-label        (#js*.document.createElement #js"label"))
  (define br-1              (#js*.document.createElement #js"br"))
  (define name-input        (#js*.document.createElement #js"input"))
  (define br-2              (#js*.document.createElement #js"br"))
  (define server-id-label   (#js*.document.createElement #js"label"))
  (define br-3              (#js*.document.createElement #js"br"))
  (define server-id-input   (#js*.document.createElement #js"input"))
  (define br-4              (#js*.document.createElement #js"br"))
  (define form-submit       (#js*.document.createElement #js"input"))
  
  (define hr                (#js*.document.createElement #js"hr"))
  (define universe-button   (#js*.document.createElement #js"button"))

  (:= #js.name-label.innerHTML        #js"Username")
  (:= #js.server-id-label.innerHTML   #js"Universe's Peer ID")
  (:= #js.name-input.placeholder      #js"michael1234")
  (:= #js.server-id-input.placeholder (js-string (generate-id)))
  (:= #js.form-submit.type            #js"submit")
  (:= #js.form-submit.value           #js"Join!")
  
  (:= #js.universe-button.innerHTML   #js"Start Universe")

  (for-each (λ (el)
              (#js.join-form.appendChild el)
              0)
            (list name-label br-1 name-input
                  br-2
                  server-id-label br-3 server-id-input
                  br-4
                  form-submit))

  (:= #js.join-form.onsubmit 
      (λ ()
        (big-bang-callback (js-string->string #js.name-input.value)
                           (js-string->string #js.server-id-input.value)
                           root)
        (#js.container.remove)))
  
  (:= #js.universe-button.onclick
      (λ ()
        (universe-callback root)
        (#js.container.remove)))

  (#js.container.appendChild join-form)
  (#js.container.appendChild hr)
  (#js.container.appendChild universe-button)

  (#js.root.appendChild container))