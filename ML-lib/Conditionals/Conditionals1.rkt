#lang racket
(require (prefix-in #% "../Numbers/Numbers1.rkt")
         "../ML-Helpers.rkt"
         (prefix-in #% "ConditionalsSetup.rkt"))
(provide
 (unprefix-out #% "../Numbers/Numbers1.rkt")               
 (all-from-out "ConditionalsSetup.rkt")
 (rename-out [my-if #%if])
 true
 false)

;0 "" "0" and #f are falsy
(define (truthy-value expr)
  (nor (equal? expr 0) (equal? expr "") (equal? expr "0") (not expr)))

(define-syntax-rule (my-if cond then else)
 (if (truthy-value cond) then else))


