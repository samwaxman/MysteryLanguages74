#lang racket
;THIS LANGUAGE IS THE NORMAL RACKET BEHAVIOR FOR FUNCTION CALLS
;ITS ONLY DIFFERENCE FROM NAMED FUNCTIONS1 IS THAT IT HAS THE
;SAME NOTION OF "FUEL" AS THE OTHER LANGUAGES, AND CAN TIME OUT.


(require (prefix-in Cond- "../Conditionals/Conditionals3.rkt")
         (only-in "../Scope/Scope1.rkt" #%reassign)
         "../ML-Helpers.rkt")

(provide
 (except-out
  (unprefix-out Cond- "../Conditionals/Conditionals3.rkt"))
 #%func
 #%reassign)

;;This is the exact same language as NamedFunctions1, but with fuel
;; and reassignment
(define-syntax-rule (#%func name (args ...) body ... last-body)
  (define name (lambda (args ...) (check-decrement-fuel) body ... last-body)))

