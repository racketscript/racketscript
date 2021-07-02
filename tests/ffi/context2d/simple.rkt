#lang racketscript/base

(require racketscript/interop)

(define document ($$ 'window.document))

(define (onload)
  (define canvas ($$ document.getElementById #js"main_canvas"))
  (define ctx ($$ canvas.getContext #js"2d"))
  ($/:= #js.ctx.fillStyle #js"blue")
  ($$ ctx.fillRect 10 20 200 100)
  ($$ ctx.strokeStyle "#fa00ff")
  ($$ ctx.lineWidth 5)
  ($$ ctx.arc 50 50 20 20 ($ 'Math 'PI) #f)
  ($$ ctx.stroke))

(onload)
