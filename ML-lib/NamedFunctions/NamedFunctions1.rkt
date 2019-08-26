#lang racket
(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         "../ML-Helpers.rkt")

(provide
 (unprefix-out Cond- "../Conditionals/Conditionals3.rkt")
 (rename-out [func #%func]))


;Standard function definition.
(define-syntax-rule (func name (args ...) body ... last-body)
  (define (name args ...) (check-decrement-fuel) body ... last-body))
