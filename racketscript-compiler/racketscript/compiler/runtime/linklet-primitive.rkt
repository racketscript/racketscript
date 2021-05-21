#lang racketscript/boot

(require racketscript/interop "lib.rkt" "syntax.rkt")

(define+provide (variable-reference-from-unsafe? v) #false)
(define+provide (variable-reference-constant? v) #false)
(define+provide make-instance #js.Core.Linklet.makeInstance)
(define+provide instance-data #js.Core.Linklet.instanceData)
(define+provide instance-name #js.Core.Linklet.instanceName)
(define+provide instance-variable-value #js.Core.Linklet.instanceVariableValue)
(define+provide instance-variable-names #js.Core.Linklet.instanceVariableNames)
(define+provide instance-set-variable-value! #js.Core.Linklet.instanceSetVariableValue)
(define+provide instance-unset-variable! #js.Core.Linklet.instanceUnsetVariable)
(define+provide instance-describe-variable! #js.Core.Linklet.instanceDescribeVariable)
(define+provide (linklet-virtual-machine-bytes) #"racketscript")

(define-syntax-rule (bounce ids ...)
  (#js.Core.Hash.makeEqual
   (array (array 'ids ids) ...)
   #false))

;; one big table with everything
(define+provide (primitive-table v)
  (bounce syntax? syntax-e datum->syntax syntax->datum
          syntax-property syntax-property-symbol-keys
          syntax-source syntax-line syntax-column syntax-span
          syntax-position
          variable-reference-constant?
          variable-reference-from-unsafe?
          make-instance
          instance-describe-variable!
          instance-unset-variable!
          instance-set-variable-value!
          instance-variable-names
          instance-variable-value
          instance-data
          instance-name
          linklet-virtual-machine-bytes
          primitive-table))
  
