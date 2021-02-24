#lang racketscript/boot

(require racketscript/interop "lib.rkt" "syntax.rkt")

(define+provide (variable-reference-from-unsafe? v) #false)
(define+provide (variable-reference-constant? v) #false)
(define+provide make-instance #js.Core.Linklet.make_instance)
(define+provide instance-data #js.Core.Linklet.instance_data)
(define+provide instance-name #js.Core.Linklet.instance_name)
(define+provide instance-variable-value #js.Core.Linklet.instance_variable_value)
(define+provide instance-variable-names #js.Core.Linklet.instance_variable_names)
(define+provide instance-set-variable-value! #js.Core.Linklet.instance_set_variable_value)
(define+provide instance-unset-variable! #js.Core.Linklet.instance_unset_variable)
(define+provide instance-describe-variable! #js.Core.Linklet.instance_describe_variable)
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
  
